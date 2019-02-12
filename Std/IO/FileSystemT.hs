{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Std.IO.FileSystemT
Description : Filesystem I/O using threadpool
Copyright   : (c) Winterland, 2017~2019
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide I/O operations related to filesystem, operations are implemented using libuv's threadpool to achieve non-block behavior (non-block here meaning won't block other haskell threads), which should be prefered when the operations' estimated time is long enough(>1ms) or running with a non-threaded haskell runtime, such as accessing network filesystem or scan a very large directory. Otherwise you may block RTS's capability thus all the other haskell threads live on it.

The threadpool version operations have overheads similar to safe FFI, but provide same adventages:

  * The libuv's threadpool have a limit on concurrent threads number (4 by default), which can reduce disk contention.
  * The threadpool version works with non-threaded runtime, which doesn't have safe FFI available.
  * The threadpool version won't relinquish current HEC (Haskell Execution Context) a.k.a. capability.

-}

module Std.IO.FileSystemT
  ( -- * regular file devices
    UVFile
  , UVFileReader, newUVFileReader, peekUVFileReader
  , UVFileWriter, newUVFileWriter, peekUVFileWriter
  , initUVFile
    -- * opening constant
  , UVFileMode(DEFAULT_MODE, S_IRWXU, S_IRUSR, S_IWUSR
      , S_IXUSR, S_IRWXG, S_IRGRP, S_IWGRP, S_IXGRP, S_IRWXO, S_IROTH
      )
  , UVFileFlag(O_APPEND, O_CREAT, O_DIRECT, O_DSYNC, O_EXCL
      , O_EXLOCK, O_NOATIME, O_NOFOLLOW, O_RDONLY, O_RDWR, O_SYMLINK
      , O_SYNC, O_TRUNC, O_WRONLY, O_RANDOM, O_SHORT_LIVED, O_SEQUENTIAL, O_TEMPORARY
      )
  -- * filesystem operations
  , mkdir
  , unlink
  , mkdtemp
  , rmdir
  , DirEntType(..)
  , scandir
  , UVStat(..), UVTimeSpec(..)
  , stat, lstat, fstat
  , rename
  , fsync, fdatasync
  , ftruncate
  , UVCopyFileFlag(COPYFILE_DEFAULT, COPYFILE_EXCL, COPYFILE_FICLONE)
  , copyfile
  , UVAccessMode(F_OK, R_OK, W_OK, X_OK)
  , AccessResult(..)
  , access
  , chmod, fchmod
  , utime, futime
  , UVSymlinkFlag(SYMLINK_DEFAULT, SYMLINK_DIR, SYMLINK_JUNCTION)
  , link, symlink
  , readlink, realpath
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Word
import           Data.Int
import           GHC.Generics
import           Std.Data.CBytes                 as CBytes
import           Foreign.Ptr
import           Foreign.Storable               (peekElemOff)
import           Foreign.Marshal.Alloc          (allocaBytes)
import           Std.Foreign.PrimArray          (withPrimSafe', withPrimUnsafe')
import           Std.IO.Buffered
import           Std.IO.Exception
import           Std.IO.Resource
import           Std.IO.UV.Errno
import           Std.IO.UV.FFI
import           Std.IO.UV.Manager

--------------------------------------------------------------------------------
-- File

-- | 'UVFile' wrap a @uv_file_t@ and a referencing counter
--
-- Note this is a differet data type from "Std.IO.FileSystem" 's one, the 'Input'
-- and 'Output' instance use thread pool version functions.
--
-- libuv implements read and write method with both implict and explict offset capability.
-- (negative offset result in @read/write@ system call otherwise @pread/pwrite@), we provide
-- implict offset interface with 'UVFile', which is NOT thread safe.
--
-- An offset bundled 'UVFileReader', 'UVFileWriter' is also provided, which can be used
-- concurrently. The offset is protected with 'MVar' and increasing automatically.
data UVFile = UVFile
    { uvfFD      :: {-# UNPACK #-} !UVFD
    , uvfCounter :: {-# UNPACK #-} !(TVar Int)
    }

instance Show UVFile where
    show (UVFile fd _) = "Std.IO.FileSystemT: UVFile" ++ show fd

instance Input UVFile where
    readInput f buf bufSiz = readUVFile f buf bufSiz (-1)

readUVFile :: HasCallStack => UVFile -> Ptr Word8 -> Int -> Int64 -> IO Int
readUVFile (UVFile fd counter) buf bufSiz off =
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (do uvm <- getUVManager
                 withUVRequest uvm
                    (hs_uv_fs_read_threaded fd buf bufSiz off))

instance Output UVFile where
    writeOutput f buf bufSiz = writeUVFile f buf bufSiz (-1)

writeUVFile :: HasCallStack => UVFile -> Ptr Word8 -> Int -> Int64 -> IO ()
writeUVFile (UVFile fd counter) buf bufSiz off =
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (if off == -1 then go buf bufSiz
                           else go' buf bufSiz off)
  where
    -- use -1 offset to use fd's default offset
    go buf bufSiz = do
        uvm <- getUVManager
        written <- withUVRequest uvm
            (hs_uv_fs_write_threaded fd buf bufSiz (-1))
        when (written < bufSiz)
            (go (buf `plusPtr` written) (bufSiz-written))

    go' buf bufSiz !off = do
        uvm <- getUVManager
        written <- withUVRequest uvm
            (hs_uv_fs_write_threaded fd buf bufSiz off)
        when (written < bufSiz) $
            go' (buf `plusPtr` written)
                (bufSiz-written)
                (off+fromIntegral written)

data UVFileReader = UVFileReader {-# UNPACK #-} !UVFile
                                 {-# UNPACK #-} !(MVar Int64)

-- |  Create a reader from an 'UVFile'.
--
-- Note this will not increase 'UVFile''s referencing counter.
newUVFileReader :: UVFile       -- ^ the file we're reading
                -> Int64        -- ^ initial reading offset
                -> IO UVFileReader
newUVFileReader uvf off = UVFileReader uvf <$> newMVar off

-- | Change reader's offset.
peekUVFileReader :: UVFileReader
                 -> Int64       -- ^ the new offset
                 -> IO Int64    -- ^ the old offset
peekUVFileReader (UVFileReader _ offsetLock) = swapMVar offsetLock

instance Input UVFileReader where
    readInput (UVFileReader file offsetLock) buf bufSiz =
        modifyMVar offsetLock $ \ off -> do
            !l <- readUVFile file buf bufSiz off
            let !off' = off + fromIntegral l
            return (off', l)


data UVFileWriter = UVFileWriter {-# UNPACK #-} !UVFile
                                 {-# UNPACK #-} !(MVar Int64)

-- | Create a writer from an 'UVFile'.
--
-- Note this will not increase 'UVFile''s referencing counter.
newUVFileWriter :: UVFile       -- ^ the file we're writing
                -> Int64        -- ^ initial writing offset
                -> IO UVFileWriter
newUVFileWriter uvf off = UVFileWriter uvf <$> newMVar off

-- | Change writer's offset.
peekUVFileWriter :: UVFileWriter
                 -> Int64       -- ^ the new offset
                 -> IO Int64    -- ^ the old offset
peekUVFileWriter (UVFileWriter _ offsetLock) = swapMVar offsetLock

instance Output UVFileWriter where
    writeOutput (UVFileWriter file offsetLock) buf bufSiz =
        modifyMVar_ offsetLock $ \ off -> do
            writeUVFile file buf bufSiz off
            let !off' = off + fromIntegral bufSiz
            return off'

--------------------------------------------------------------------------------

-- | init a file 'Resource', which open a file when used.
--
-- Resource closing will wait for the referencing counter goes
-- down to zero (no reading or writing is in process), which can
-- be a problem if you are using multiple readers or writers in multiple threads.
-- In that case you have to stop all reading or writing thread if you don't want to
-- block the resource thread.
initUVFile :: HasCallStack
           => CBytes
           -> UVFileFlag        -- ^ Opening flags, e.g. 'O_CREAT' @.|.@ 'O_RDWR'
           -> UVFileMode        -- ^ Sets the file mode (permission and sticky bits),
                                -- but only if the file was created, see 'DEFAULT_MODE'.
           -> Resource UVFile
initUVFile path flags mode =
    initResource
        (do uvm <- getUVManager
            fd <- withCBytes path $ \ p ->
                withUVRequest uvm (hs_uv_fs_open_threaded p flags mode)
            counter <- newTVarIO 0
            return (UVFile (fromIntegral fd) counter))
        (\ (UVFile fd counter) -> join . atomically $ do
            s <- readTVar counter
            case s `compare` 0 of
                GT -> retry -- don't close until no one is using it
                EQ -> do swapTVar counter (-1)
                         -- there's no need to wait for closing finish
                         -- so just put the closing into the thread pool
                         return (do
                            uvm <- getUVManager
                            void . withUVRequest uvm $
                                hs_uv_fs_close_threaded fd)
                LT -> return (return ()))

--------------------------------------------------------------------------------

-- | Equivalent to <http://linux.die.net/man/2/mkdir mkdir(2)>.
--
-- Note mode is currently not implemented on Windows.
mkdir :: HasCallStack => CBytes -> UVFileMode -> IO ()
mkdir path mode = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withUVRequest_ uvm (hs_uv_fs_mkdir_threaded p mode)

-- | Equivalent to <http://linux.die.net/man/2/unlink unlink(2)>.
unlink :: HasCallStack => CBytes -> IO ()
unlink path = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withUVRequest_ uvm (hs_uv_fs_unlink_threaded p)

-- | Equivalent to <mkdtemp http://linux.die.net/man/3/mkdtemp>
--
-- Creates a temporary directory in the most secure manner possible.
-- There are no race conditions in the directory’s creation.
-- The directory is readable, writable, and searchable only by the creating user ID.
-- The user of mkdtemp() is responsible for deleting the temporary directory and
-- its contents when done with it.
--
-- Note: the argument is the prefix of the temporary directory,
-- so no need to add XXXXXX ending.
mkdtemp :: HasCallStack => CBytes -> IO CBytes
mkdtemp path = do
    let size = CBytes.length path
    withCBytes path $ \ p ->
        CBytes.create (size+7) $ \ p' -> do  -- we append "XXXXXX\NUL" in C
            uvm <- getUVManager
            withUVRequest_ uvm (hs_uv_fs_mkdtemp_threaded p size p')
            return (size+6)

-- | Equivalent to <http://linux.die.net/man/2/rmdir rmdir(2)>.
rmdir :: HasCallStack => CBytes -> IO ()
rmdir path = do
    uvm <- getUVManager
    withCBytes path (void . withUVRequest uvm . hs_uv_fs_rmdir_threaded)

--------------------------------------------------------------------------------

-- | Equivalent to <http://linux.die.net/man/3/scandir scandir(3)>.
--
-- Note Unlike scandir(3), this function does not return the “.” and “..” entries.
--
-- Note On Linux, getting the type of an entry is only supported by some file systems (btrfs, ext2, ext3 and ext4 at the time of this writing), check the <http://linux.die.net/man/2/getdents getdents(2)> man page.
scandir :: HasCallStack => CBytes -> IO [(CBytes, DirEntType)]
scandir path = do
    uvm <- getUVManager
    bracket
        (withCBytes path $ \ p ->
            withPrimSafe' $ \ dents ->
                withUVRequestEx uvm
                    (hs_uv_fs_scandir_threaded p dents)
                    (hs_uv_fs_scandir_extra_cleanup dents))
        (\ (dents, n) -> hs_uv_fs_scandir_cleanup dents n)
        (\ (dents, n) -> forM [0..n-1] $ \ i -> do
            dent <- peekElemOff dents i
            (path, typ) <- peekUVDirEnt dent
            let !typ' = fromUVDirEntType typ
            !path' <- fromCString path
            return (path', typ'))

--------------------------------------------------------------------------------

-- | Equivalent to <http://linux.die.net/man/2/stat stat(2)>
stat :: HasCallStack => CBytes -> IO UVStat
stat path = do
    withCBytes path $ \ p ->
         allocaBytes uvStatSize $ \ stat -> do
            uvm <- getUVManager
            withUVRequest_ uvm (hs_uv_fs_stat_threaded p stat)
            peekUVStat stat

-- | Equivalent to <http://linux.die.net/man/2/lstat lstat(2)>
lstat :: HasCallStack => CBytes -> IO UVStat
lstat path = do
    withCBytes path $ \ p ->
         allocaBytes uvStatSize $ \ stat -> do
            uvm <- getUVManager
            withUVRequest_ uvm (hs_uv_fs_lstat_threaded p stat)
            peekUVStat stat

-- | Equivalent to <http://linux.die.net/man/2/fstat fstat(2)>
fstat :: HasCallStack => UVFile -> IO UVStat
fstat (UVFile fd counter) = do
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (allocaBytes uvStatSize $ \ stat -> do
                uvm <- getUVManager
                withUVRequest_ uvm (hs_uv_fs_fstat_threaded fd stat)
                peekUVStat stat)

--------------------------------------------------------------------------------

-- | Equivalent to <http://linux.die.net/man/2/rename rename(2)>.
--
-- Note On Windows if this function fails with UV_EBUSY, UV_EPERM or UV_EACCES, it will retry to rename the file up to four times with 250ms wait between attempts before giving up. If both path and new_path are existing directories this function will work only if target directory is empty.
rename :: HasCallStack => CBytes -> CBytes -> IO ()
rename path path' = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withCBytes path' $ \ p' ->
            withUVRequest_ uvm (hs_uv_fs_rename_threaded p p')

-- | Equivalent to <http://linux.die.net/man/2/fsync fsync(2)>.
fsync :: HasCallStack => UVFile -> IO ()
fsync (UVFile fd counter) =
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (do uvm <- getUVManager
                 withUVRequest_ uvm (hs_uv_fs_fsync_threaded fd))

-- | Equivalent to <http://linux.die.net/man/2/fdatasync fdatasync(2)>.
fdatasync :: HasCallStack => UVFile -> IO ()
fdatasync (UVFile fd counter) =
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (do uvm <- getUVManager
                 withUVRequest_ uvm (hs_uv_fs_fdatasync_threaded fd))

-- | Equivalent to <http://linux.die.net/man/2/ftruncate ftruncate(2)>.
ftruncate :: HasCallStack => UVFile -> Int64 -> IO ()
ftruncate (UVFile fd counter) off =
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (do uvm <- getUVManager
                 withUVRequest_ uvm (hs_uv_fs_ftruncate_threaded fd off))

-- | Copies a file from path to new_path.
--
-- Warning: If the destination path is created, but an error occurs while copying the data, then the destination path is removed. There is a brief window of time between closing and removing the file where another process could access the file.
copyfile :: HasCallStack => CBytes -> CBytes -> UVCopyFileFlag -> IO ()
copyfile path path' flag = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withCBytes path' $ \ p' ->
            withUVRequest_ uvm (hs_uv_fs_copyfile_threaded p p' flag)

-- | Equivalent to <http://linux.die.net/man/2/access access(2)> on Unix.
-- Windows uses GetFileAttributesW().
access :: HasCallStack => CBytes -> UVAccessMode -> IO AccessResult
access path mode = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withUVRequest' uvm (hs_uv_fs_access_threaded p mode) (handleResult . fromIntegral)
  where
    handleResult r
        | r == 0           = return AccessOK
        | r == UV_ENOENT   = return NoExistence
        | r == UV_EACCES   = return NoPermission
        | otherwise        = do
            name <- uvErrName r
            desc <- uvStdError r
            throwUVError r (IOEInfo name desc callStack)

-- | Equivalent to <http://linux.die.net/man/2/chmod chmod(2)>.
chmod :: HasCallStack => CBytes -> UVFileMode -> IO ()
chmod path mode = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withUVRequest_ uvm (hs_uv_fs_chmod_threaded p mode)

-- | Equivalent to <http://linux.die.net/man/2/fchmod fchmod(2)>.
fchmod :: HasCallStack => UVFile -> UVFileMode -> IO ()
fchmod (UVFile fd counter) mode =
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (do uvm <- getUVManager
                 withUVRequest_ uvm (hs_uv_fs_fchmod_threaded fd mode))

-- | Equivalent to <http://linux.die.net/man/2/utime utime(2)>.
--
-- libuv choose 'Double' type due to cross platform concerns, we only provide micro-second precision:
--
--   * second     = v
--   * nanosecond = (v * 1000000) % 1000000 * 1000;
--
-- second and nanosecond are fields in 'UVTimeSpec' respectively.
--
-- Note libuv prior to v1.23.1 have issues which may result in nanosecond not set, 'futime' doesn't have
utime :: HasCallStack
      => CBytes
      -> Double     -- ^ atime, i.e. access time
      -> Double     -- ^ mtime, i.e. modify time
      -> IO ()
utime path atime mtime = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withUVRequest_ uvm (hs_uv_fs_utime_threaded p atime mtime)

-- | Equivalent to <http://linux.die.net/man/2/futime futime(2)>.
--
-- Same precision notes with 'utime'.
futime :: HasCallStack => UVFile -> Double -> Double -> IO ()
futime (UVFile fd counter) atime mtime =
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (do uvm <- getUVManager
                 withUVRequest_ uvm (hs_uv_fs_futime_threaded fd atime mtime))

-- | Equivalent to <http://linux.die.net/man/2/link link(2)>.
link :: HasCallStack => CBytes -> CBytes -> IO ()
link path path' = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withCBytes path' $ \ p' ->
            withUVRequest_ uvm (hs_uv_fs_link_threaded p p')

-- | Equivalent to <http://linux.die.net/man/2/symlink symlink(2)>.
--
-- | Note On Windows the flags parameter can be specified to control how the symlink will be created.
--
--   * 'SYMLINK_DIR': indicates that path points to a directory.
--   * 'SYMLINK_JUNCTION': request that the symlink is created using junction points.
--
-- On other platforms these flags are ignored.
symlink :: HasCallStack => CBytes -> CBytes -> UVSymlinkFlag -> IO ()
symlink path path' flag = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withCBytes path' $ \ p' ->
            withUVRequest_ uvm (hs_uv_fs_symlink_threaded p p' flag)

-- | Equivalent to <http://linux.die.net/man/2/readlink readlink(2)>.
readlink :: HasCallStack => CBytes -> IO CBytes
readlink path = do
    uvm <- getUVManager
    bracket
        (withCBytes path $ \ p ->
            withPrimSafe' $ \ p' ->
                withUVRequestEx uvm
                    (hs_uv_fs_readlink_threaded p p')
                    (\ _ -> hs_uv_fs_readlink_extra_cleanup p'))
        (\ (path, _) -> hs_uv_fs_readlink_cleanup path)
        (\ (path, _) -> do
            !path' <- fromCString path
            return path')

-- | Equivalent to <http://linux.die.net/man/3/realpath realpath(3)> on Unix. Windows uses <https://msdn.microsoft.com/en-us/library/windows/desktop/aa364962(v=vs.85).aspx GetFinalPathNameByHandle>.
--
-- Warning This function has certain platform-specific caveats that were discovered when used in Node.
--
--  * macOS and other BSDs: this function will fail with UV_ELOOP if more than 32 symlinks are found while
--    resolving the given path. This limit is hardcoded and cannot be sidestepped.
--
--  * Windows: while this function works in the common case, there are a number of corner cases where it doesn’t:
--
--      * Paths in ramdisk volumes created by tools which sidestep the Volume Manager (such as ImDisk) cannot be resolved.
--      * Inconsistent casing when using drive letters.
--      * Resolved path bypasses subst’d drives.
--
-- While this function can still be used, it’s not recommended if scenarios such as the above need to be supported.
-- The background story and some more details on these issues can be checked <https://github.com/nodejs/node/issues/7726 here>.
--
-- Note This function is not implemented on Windows XP and Windows Server 2003. On these systems, UV_ENOSYS is returned.
realpath :: HasCallStack => CBytes -> IO CBytes
realpath path = do
    uvm <- getUVManager
    bracket
        (withCBytes path $ \ p ->
            withPrimSafe' $ \ p' ->
                withUVRequestEx uvm
                    (hs_uv_fs_realpath_threaded p p')
                    (\ _ -> hs_uv_fs_readlink_extra_cleanup p'))
        (\ (path, _) -> hs_uv_fs_readlink_cleanup path)
        (\ (path, _) -> do
            !path' <- fromCString path
            return path')
