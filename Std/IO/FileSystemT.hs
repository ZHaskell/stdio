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

This module provide I/O operations related to filesystem, operations are implemented using libuv's threadpool to achieve non-block behavior, which should be prefered when the operations' estimated time is long enough(>1ms) or running with a non-threaded haskell runtime, such as accessing network filesystem or scan a very large directory. Otherwise you may block RTS's capability thus all the other haskell threads live on it.

-}

module Std.IO.FileSystemT
  ( -- * regular file devices
    UVFile
  , UVFileReader, newUVFileReader, peekUVFileReader
  , UVFileWriter, newUVFileWriter, peekUVFileWriter
  , initUVFile
    -- * opening constant
  , UVFileMode
  , defaultMode
  , UVFileFlag
  , uV_FS_O_APPEND
  , uV_FS_O_CREAT
  , uV_FS_O_DIRECT
  , uV_FS_O_DIRECTORY
  , uV_FS_O_DSYNC
  , uV_FS_O_EXCL
  , uV_FS_O_EXLOCK
  , uV_FS_O_NOATIME
  , uV_FS_O_NOCTTY
  , uV_FS_O_NOFOLLOW
  , uV_FS_O_NONBLOCK
  , uV_FS_O_RDONLY
  , uV_FS_O_RDWR
  , uV_FS_O_SYMLINK
  , uV_FS_O_SYNC
  , uV_FS_O_TRUNC
  , uV_FS_O_WRONLY
  , uV_FS_O_RANDOM
  , uV_FS_O_SHORT_LIVED
  , uV_FS_O_SEQUENTIAL
  , uV_FS_O_TEMPORARY
  -- * filesystem operations
  , mkdir
  , unlink
  , mkdtemp
  , rmdir
  , DirEntType(..)
  , scandir
  , stat
  , lstat
  , fstat
  , rename
  , fsync
  , fdatasync
  , ftruncate
  , UVCopyFileFlag
  , uV_FS_COPYFILE_DEFAULT
  , uV_FS_COPYFILE_EXCL
  , uV_FS_COPYFILE_FICLONE
  , uV_FS_COPYFILE_FICLONE_FORCE
  , copyfile
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
import           Std.IO.UV.FFI
import           Std.IO.UV.Manager

--------------------------------------------------------------------------------
-- File

-- | 'UVFile' wrap a @uv_file_t@ and a referencing counter
--
-- Note this is a differet data type from "Std.IO.FileSystem" 's one, the 'Input'
-- and 'Output' instance use thread pool version functions.
--
-- libuv implements read and write method with both implict and explict offset capable.
-- (negative offset result in @read/write@ system call otherwise @pread/pwrite@), we provide
-- implict offset interface with 'UVFile', which is NOT thread safe.
--
-- An offset bundled 'UVFileReader', 'UVFileWriter' is also provided, which can be used
-- concurrently. The offset is protected with 'MVar' and increasing automatically.
data UVFile = UVFile
    { uvfFD      :: {-# UNPACK #-} !UVFD
    , uvfCounter :: {-# UNPACK #-} !(TVar Int)
    }

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

-- | Open a file and get the descriptor
initUVFile :: HasCallStack
           => CBytes
           -> UVFileFlag
           -> UVFileMode        -- ^ Sets the file mode (permission and sticky bits),
                                -- but only if the file was created, see 'defaultMode'.
           -> Resource UVFile
initUVFile path flags mode = do
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

mkdir :: HasCallStack => CBytes -> UVFileMode -> IO ()
mkdir path mode = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withUVRequest_ uvm (hs_uv_fs_mkdir_threaded p mode)

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

rmdir :: HasCallStack => CBytes -> IO ()
rmdir path = do
    uvm <- getUVManager
    withCBytes path (void . withUVRequest uvm . hs_uv_fs_rmdir_threaded)

--------------------------------------------------------------------------------

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

stat :: HasCallStack => CBytes -> IO UVStat
stat path = do
    withCBytes path $ \ p ->
         allocaBytes uvStatSize $ \ stat -> do
            uvm <- getUVManager
            withUVRequest_ uvm (hs_uv_fs_stat_threaded p stat)
            peekUVStat stat

lstat :: HasCallStack => CBytes -> IO UVStat
lstat path = do
    withCBytes path $ \ p ->
         allocaBytes uvStatSize $ \ stat -> do
            uvm <- getUVManager
            withUVRequest_ uvm (hs_uv_fs_lstat_threaded p stat)
            peekUVStat stat

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

rename :: HasCallStack => CBytes -> CBytes -> IO ()
rename path path' = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withCBytes path' $ \ p' ->
            withUVRequest_ uvm (hs_uv_fs_rename_threaded p p')

fsync :: HasCallStack => UVFile -> IO ()
fsync (UVFile fd counter) =
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (do uvm <- getUVManager
                 withUVRequest_ uvm (hs_uv_fs_fsync_threaded fd))

fdatasync :: HasCallStack => UVFile -> IO ()
fdatasync (UVFile fd counter) =
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (do uvm <- getUVManager
                 withUVRequest_ uvm (hs_uv_fs_fdatasync_threaded fd))

ftruncate :: HasCallStack => UVFile -> Int64 -> IO ()
ftruncate (UVFile fd counter) off =
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (do uvm <- getUVManager
                 withUVRequest_ uvm (hs_uv_fs_ftruncate_threaded fd off))

copyfile :: HasCallStack => CBytes -> CBytes -> UVCopyFileFlag -> IO ()
copyfile path path' flag = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withCBytes path' $ \ p' ->
            withUVRequest_ uvm (hs_uv_fs_copyfile_threaded p p' flag)
