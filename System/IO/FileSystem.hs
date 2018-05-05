{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
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

scandir :: CBytes -> IO [(CBytes, UVDirEntType)]
scandir path = withCBytes path $ \ p ->
    withResource (initUVReq uV_FS) $ \ req -> do
        uvFSScandir nullPtr req p False
        loopScanDirReq req

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
                path' <- fromCString path
                rest <- loopScanDirReq req
                return ((path', typ) : rest)

scandirT :: CBytes -> IO [(CBytes, UVDirEntType)]
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
