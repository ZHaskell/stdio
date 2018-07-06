{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImplicitParams #-}

{-|
Module      : Std.IO.FileSystem
Description : Filesystem I/O
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide I/O operations related to filesystem, operations are implemented using unsafe FFIs by default,
functions which have @T@ suffix leverage libuv's threadpool to achieve non-block behavior, which should be prefered when the operations' estimated time is long enough(>1ms), such as accessing network filesystem or scan a very large directory. Otherwise you may block RTS's capability thus all the other haskell threads live on it.

-}

module Std.IO.FileSystem
  ( -- * regular file devices
    UVFile, UVFileT
  , UVFileReader, UVFileReaderT
  , UVFileWriter, UVFileWriterT
  , initUVFile
  , initUVFileT
    -- * opening constant
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
  , UVFileMode
  , defaultMode
  -- * filesystem operations
  , mkdir
  , mkdirT
  , unlink
  , unlinkT
  , mkdtemp
  , mkdtempT
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Word
import           Data.Int
import           Std.Data.CBytes                 as CBytes
import           Foreign.Ptr
import           Std.IO.Buffered
import           Std.IO.Exception
import           Std.IO.Resource
import           Std.IO.UV.FFI
import           Std.IO.UV.Manager

--------------------------------------------------------------------------------
-- File

data UVFile = UVFile
    { uvfFD      :: {-# UNPACK #-} !UVFD
    , uvfCounter :: {-# UNPACK #-} !(TVar Int)
    }

instance Input UVFile where
    -- readInput :: HasCallStack => UVFile -> Ptr Word8 -> Int -> IO Int
    -- use -1 offset to use fd's default offset
    readInput f buf bufSiz = readUVFile f buf bufSiz (-1)

readUVFile :: HasCallStack => UVFile -> Ptr Word8 -> Int -> Int64 -> IO Int
readUVFile (UVFile fd counter) buf bufSiz off =
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (throwUVIfMinus $ hs_uv_fs_read fd buf bufSiz off)

instance Output UVFile where
    -- use -1 offset to use fd's default offset
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
    go !buf !bufSiz = do
        written <- throwUVIfMinus
            (hs_uv_fs_write fd buf bufSiz (-1))
        when (written < bufSiz)
            (go (buf `plusPtr` written) (bufSiz-written))

    go' !buf !bufSiz !off = do
        written <- throwUVIfMinus
            (hs_uv_fs_write fd buf bufSiz off)
        when (written < bufSiz) $
            go' (buf `plusPtr` written)
                (bufSiz-written)
                (off+fromIntegral written)

-- | Threaded newtype wrapper for 'UVFile'
--
newtype UVFileT = UVFileT { getUVFile :: UVFile }

instance Input UVFileT where
    readInput f buf bufSiz = readUVFileT f buf bufSiz (-1)

readUVFileT :: HasCallStack => UVFileT -> Ptr Word8 -> Int -> Int64 -> IO Int
readUVFileT (UVFileT (UVFile fd counter)) buf bufSiz off =
    bracket_ (atomically $ do
                s <- readTVar counter
                if s >= 0 then modifyTVar' counter (+1)
                          else throwECLOSEDSTM)
             (atomically $ modifyTVar' counter (subtract 1))
             (do uvm <- getUVManager
                 withUVManagerWrap uvm
                    (hs_uv_fs_read_threaded fd buf bufSiz off))

instance Output UVFileT where
    writeOutput f buf bufSiz = writeUVFileT f buf bufSiz (-1)

writeUVFileT :: HasCallStack => UVFileT -> Ptr Word8 -> Int -> Int64 -> IO ()
writeUVFileT (UVFileT (UVFile fd counter)) buf bufSiz off =
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
        written <- withUVManagerWrap uvm
            (hs_uv_fs_write_threaded fd buf bufSiz (-1))
        when (written < bufSiz)
            (go (buf `plusPtr` written) (bufSiz-written))

    go' buf bufSiz off = do
        uvm <- getUVManager
        written <- withUVManagerWrap uvm
            (hs_uv_fs_write_threaded fd buf bufSiz (-1))
        when (written < bufSiz) $ do
            go' (buf `plusPtr` written)
                (bufSiz-written)
                (off+fromIntegral written)

data UVFileReader = UVFileReader {-# UNPACK #-} !UVFile
                                 {-# UNPACK #-} !(MVar Int64)

instance Input UVFileReader where
    readInput (UVFileReader file offsetLock) buf bufSiz =
        modifyMVar offsetLock $ \ off -> do
            !l <- readUVFile file buf bufSiz off
            let !off' = off + fromIntegral l
            return (off', l)

data UVFileReaderT = UVFileReaderT {-# UNPACK #-} !UVFileT
                                   {-# UNPACK #-} !(MVar Int64)

instance Input UVFileReaderT where
    readInput (UVFileReaderT file offsetLock) buf bufSiz =
        modifyMVar offsetLock $ \ off -> do
            !l <- readUVFileT file buf bufSiz off
            let !off' = off + fromIntegral l
            return (off', l)

data UVFileWriter = UVFileWriter {-# UNPACK #-} !UVFile
                                 {-# UNPACK #-} !(MVar Int64)

instance Output UVFileWriter where
    writeOutput (UVFileWriter file offsetLock) buf bufSiz =
        modifyMVar_ offsetLock $ \ off -> do
            writeUVFile file buf bufSiz off
            let !off' = off + fromIntegral bufSiz
            return off'

data UVFileWriterT = UVFileWriterT {-# UNPACK #-} !UVFileT
                                   {-# UNPACK #-} !(MVar Int64)

instance Output UVFileWriterT where
    writeOutput (UVFileWriterT file offsetLock) buf bufSiz =
        modifyMVar_ offsetLock $ \ off -> do
            writeUVFileT file buf bufSiz off
            let !off' = off + fromIntegral bufSiz
            return off'

--------------------------------------------------------------------------------

-- | Default mode for open, 0o666(readable and writable).
defaultMode :: UVFileMode
defaultMode = 0o666

-- | Open a file and get the descriptor
initUVFile :: HasCallStack
     => CBytes
     -> UVFileFlag
     -> UVFileMode
     -- ^ Sets the file mode (permission and sticky bits),
     -- but only if the file was created, see 'defaultMode'.
     -> Resource UVFile
initUVFile path flags mode = do
    initResource
        (do fd <- withCBytes path $ \ p ->
                throwUVIfMinus $ hs_uv_fs_open p flags mode
            counter <- newTVarIO 0
            return (UVFile fd counter))
        (\ (UVFile fd counter) -> join . atomically $ do
            s <- readTVar counter
            case s `compare` 0 of
                GT -> retry -- don't close until no one is using it
                EQ -> do swapTVar counter (-1)
                         return (void $ hs_uv_fs_close fd)
                LT -> return (return ()))

-- | Open a file and get the descriptor
initUVFileT :: HasCallStack
     => CBytes
     -> UVFileFlag
     -> UVFileMode
     -- ^ Sets the file mode (permission and sticky bits),
     -- but only if the file was created, see 'defaultMode'.
     -> Resource UVFileT
initUVFileT path flags mode = do
    initResource
        (do uvm <- getUVManager
            fd <- withCBytes path $ \ p ->
                withUVManagerWrap uvm (hs_uv_fs_open_threaded p flags mode)
            counter <- newTVarIO 0
            return (UVFileT (UVFile (fromIntegral fd) counter)))
        (\ (UVFileT (UVFile fd counter)) -> join . atomically $ do
            s <- readTVar counter
            case s `compare` 0 of
                GT -> retry -- don't close until no one is using it
                EQ -> do swapTVar counter (-1)
                         -- there's no need to wait for closing finish
                         -- so just put the closing into the thread pool
                         return (do
                            uvm <- getUVManager
                            void . withUVManagerWrap uvm $
                                hs_uv_fs_close_threaded fd)
                LT -> return (return ()))

--------------------------------------------------------------------------------

mkdir :: HasCallStack => CBytes -> UVFileMode -> IO ()
mkdir path mode = throwUVIfMinus_ . withCBytes path $ \ p ->
     hs_uv_fs_mkdir p mode

mkdirT :: HasCallStack => CBytes -> UVFileMode -> IO ()
mkdirT path mode = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        void $ withUVManager uvm (hs_uv_fs_mkdir_threaded p mode)

unlink :: HasCallStack => CBytes -> IO ()
unlink path = throwUVIfMinus_ (withCBytes path hs_uv_fs_unlink)

unlinkT :: HasCallStack => CBytes -> IO ()
unlinkT path = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        void $ withUVManager uvm (hs_uv_fs_unlink_threaded p)

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
--
mkdtemp :: HasCallStack => CBytes -> IO CBytes
mkdtemp path = do
    let size = CBytes.length path
    withCBytes path $ \ p ->
        CBytes.create (size+7) $ \ p' -> do  -- we append "XXXXXX\NUL" in C
            throwUVIfMinus_ (hs_uv_fs_mkdtemp p size p')
            return (size+6)

mkdtempT :: HasCallStack => CBytes -> IO CBytes
mkdtempT path = do
    let size = CBytes.length path
    withCBytes path $ \ p ->
        CBytes.create (size+7) $ \ p' -> do  -- we append "XXXXXX\NUL" in C
            uvm <- getUVManager
            void $ withUVManager uvm (hs_uv_fs_mkdtemp_threaded p size p')
            return (size+6)

-- | Resource wrapper for 'mkdtemp' with automatically removal.
--
-- initTempDir :: CBytes -> Resource CBytes


{-

--------------------------------------------------------------------------------
--
--
data DirEntType
    = DirEntUnknown
    | DirEntFile
    | DirEntDir
    | DirEntLink
    | DirEntFIFO
    | DirEntSocket
    | DirEntChar
    | DirEntBlock
  deriving (Read, Show, Eq, Ord, Generic)

fromUVDirEntType :: UVDirEntType -> DirEntType
fromUVDirEntType t
    | t == uV_DIRENT_FILE   = DirEntFile
    | t == uV_DIRENT_DIR    = DirEntDir
    | t == uV_DIRENT_LINK   = DirEntLink
    | t == uV_DIRENT_FIFO   = DirEntFIFO
    | t == uV_DIRENT_SOCKET = DirEntSocket
    | t == uV_DIRENT_CHAR   = DirEntChar
    | t == uV_DIRENT_BLOCK  = DirEntBlock
    | otherwise             = DirEntUnknown

scandir :: CBytes -> IO [(CBytes, DirEntType)]
scandir path = withCBytes path $ \ p ->
    withResource (initUVReqNoSlot uV_FS) $ \ req -> do
        uvFSScandir p False nullPtr req
        loopScanDirReq req

scandirT :: CBytes -> IO [(CBytes, DirEntType)]
scandirT path = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withResource (initUVReq uV_FS (uvFSScandir p True) uvm) $ \ req -> do
            slot <- peekUVReqData req
            lock <- getBlockMVar uvm slot
            takeMVar lock
            loopScanDirReq req

loopScanDirReq :: Ptr UVReq -> IO [(CBytes, DirEntType)]
loopScanDirReq req = do
    withResource initUVDirEnt $ \ ent -> do
        r <- uv_fs_scandir_next req ent
        if r == uV_EOF
        then return []
        else if r < 0
            then do
                throwUVIfMinus_ $ return r
                return []
            else do
                (path, typ) <- peekUVDirEnt ent
                let !typ' = fromUVDirEntType typ
                !path' <- fromCString path
                !rest <- loopScanDirReq req
                return ((path', typ') : rest)
-}
