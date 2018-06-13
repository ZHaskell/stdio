{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImplicitParams #-}

{-|
Module      : System.IO.FileSystem
Description : Buffered I/O interface
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide I/O operations related to filesystem, operations are implemented using unsafe FFIs by default,
functions which have @T@ suffix leverage libuv's threadpool to achieve non-block behavior, which should be prefered when
the operations' estimated time is long enough(>1ms), such as accessing network filesystem or scan a very large directory.
Otherwise you may block RTS's capability thus all the other threads live on it.

-}

module System.IO.FileSystem
  ( UVFileFlag(..)
  , UVFileMode
  , defaultMode
  -- * Operations
  , open
  , read
  , readT
  , write
  ) where

import Prelude hiding (read)

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Word (Word8)
import System.IO (FilePath)
import System.IO.Exception
import System.IO.UV.Manager
import System.IO.UV.Internal
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.C

--------------------------------------------------------------------------------
-- File

-- | Default mode for open, 0o666(readable and writable).
defaultMode :: UVFileMode
defaultMode = 0o666

-- | Open a file and get the descriptor
open :: HasCallStack
     => FilePath
     -> UVFileFlag
     -> UVFileMode
     -- ^ Sets the file mode (permission and sticky bits),
     -- but only if the file was created, see 'defaultMode'.
     -> Resource UVFD
open path flags mode = do
    path' <- initResource (newCString path) free
    initResource
        (hs_uv_fs_open path' flags mode)
        (void . hs_uv_fs_close)

-- | Read a file, non-threaded version
read :: UVFD
     -> Int
     -> Int
     -> Resource (Ptr Word8)
read fd size offset = do
    let size' = fromIntegral size
        offset' = fromIntegral offset
    buf <- initResource (mallocBytes size :: IO (Ptr Word8)) free
    liftIO $ throwUVIfMinus_ $ hs_uv_fs_read fd buf size' offset'
    return buf

-- | Read a file, threaded version
-- The buffer shouldn't be read before the request is completed
readT :: UVManager
      -> UVFD
      -> Int
      -> Int
      -> Resource (Ptr UVFSReq, Ptr Word8)
readT mgr fd size offset = do
    let size' = fromIntegral size
        offset' = fromIntegral offset
    req <- initUVFS (\_ _ _ -> return ()) mgr
    buf <- initResource (mallocBytes size :: IO (Ptr Word8)) free
    liftIO $ withUVManager mgr $ \loop ->
        throwUVIfMinus_ $ hs_uv_fs_read_threaded loop req fd buf size' offset'
    return (req, buf)

-- | Read a file, non-threaded version
write :: UVFD
      -> Ptr Word8
      -> Int
      -> Int
      -> IO ()
write fd buf size offset = do
    let size' = fromIntegral size
        offset' = fromIntegral offset
    throwUVIfMinus_ $ hs_uv_fs_write fd buf size' offset'

{-

data UVFile = UVFile
    { uvFileFd     :: {-# UNPACK #-} UVFD
    , uvFileReq    :: {-# UNPACK #-} UVReq
    , uvFileSlot   :: {-# UNPACK #-} Int
    }

newtype UVFileT = UVFileT { uvFile :: UVFile }

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
