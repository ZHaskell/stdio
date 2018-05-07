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
  ( scandir
  , scandirT
  ) where

import System.IO.Exception
import System.IO.UV.Manager
import System.IO.UV.Internal
import System.IO.UV.Exception
import Control.Concurrent
import Foreign.Ptr
import Foreign.C
import Data.CBytes
import GHC.Generics

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
    withResource (initUVReq uV_FS) $ \ req -> do
        uvFSScandir nullPtr req p False
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

scandirT :: CBytes -> IO [(CBytes, DirEntType)]
scandirT path = do
    uvm <- getUVManager
    withCBytes path $ \ p ->
        withResource (initUVSlot uvm) $ \ slot ->
            withResource (initUVReq uV_FS) $ \ req -> do
                lock <- getBlockMVar uvm slot
                pokeUVReqData req slot
                withUVManager uvm $ \ loop -> uvFSScandir loop req p True
                takeMVar lock
                loopScanDirReq req
