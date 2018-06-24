{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : System.IO.TCP
Description : TCP servers and clients
Copyright   : (c) Winterland, 2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provides an API for creating TCP servers and clients.

-}

module System.IO.TCP (
  -- * TCP Client
    ClientConfig(..)
  , defaultClientConfig
  , initClient
  -- * TCP Server
  , ServerConfig(..)
  , defaultServerConfig
  , startServer
  , module System.IO.Net.SockAddr
  ) where

import System.IO.Net.SockAddr
import System.IO.Exception
import System.IO.Buffered
import System.IO.UV.Manager
import System.IO.UV.Internal
import Control.Concurrent.MVar
import Foreign.Ptr
import GHC.Ptr
import Foreign.C.Types (CInt(..), CSize(..))
import Data.Int
import Data.Vector
import Data.Array
import Data.IORef.Unboxed
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Foreign.PrimArray
import System.IO.Resource

--------------------------------------------------------------------------------

-- | A TCP client configuration
--
data ClientConfig = ClientConfig
    { clientLocalAddr :: Maybe SockAddr
    , clientTargetAddr :: SockAddr
    , clientNoDelay :: Bool
    }

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig Nothing (SockAddrInet 8888 inetLoopback) True

initClient :: HasCallStack => ClientConfig -> Resource UVStream
initClient ClientConfig{..} = do
    uvm <- liftIO getUVManager
    client <- initTCPStream uvm
    targetPtr <- initSockAddr clientTargetAddr
    req <- initUVReq uV_CONNECT
            (connect clientNoDelay (uvsHandle client) clientLocalAddr targetPtr)  uvm
    liftIO $ do
        slot <- peekUVReqData req
        takeMVar =<< getBlockMVar uvm slot
        throwUVIfMinus_ $ peekBufferTable uvm slot
        return client
  where
    connect nodelay handle local targetPtr loop req = do
        forM_ local $ \ local' -> withSockAddr local' $ \ localPtr ->
            throwUVIfMinus_ (uv_tcp_bind handle localPtr 0)
        throwUVIfMinus_ (hs_uv_tcp_connect req handle targetPtr)
        when nodelay $ throwUVIfMinus_ (uv_tcp_nodelay handle 1)

--------------------------------------------------------------------------------

-- | A TCP server configuration
--
data ServerConfig = ServerConfig
    { serverAddr       :: SockAddr
    , serverBackLog    :: Int
    , serverWorker     :: UVStream -> IO ()
    , serverWorkerNoDelay :: Bool
    , serverReusePortIfAvailable :: Bool    -- If this flag is enable and we're on linux >= 3.9
                                            -- We'll open multiple listening socket using SO_REUSEPORT
                                            -- otherwise this flag have no effects.
                                            --
                                            -- Depend on your workload, this option may have or not have
                                            -- improvement, it's OFF in 'defaultServerConfig'.
    }

-- | A default hello world server on localhost:8888
--
-- Test it with @main = startServer defaultServerConfig@, now try @nc -v 127.0.0.1 8888@
--
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
    (SockAddrInet 8888 inetAny)
    128
    (\ uvs -> do
        withResource (newBufferedOutput uvs 0) $ \ o ->
            writeBytes o [vASCII|hello world|])
    True
    False

-- | Start a server
--
-- Fork new worker thread upon a new connection.
--
startServer :: ServerConfig -> IO ()
startServer ServerConfig{..} = do
    if (serverReusePortIfAvailable && sO_REUSEPORT_LOAD_BALANCE == 1)
    then multipleAcceptLoop
    else singleAcceptLoop
  where
    singleAcceptLoop = do
        serverManager <- getUVManager
        withResource (initTCPStream serverManager) $ \ server -> do

            let serverHandle  = uvsHandle server
            serverSlot <- peekUVHandleData serverHandle

            withResource (initUVHandleNoSlot
                uV_CHECK
                (\ loop handle -> throwUVIfMinus_ $ hs_uv_accept_check_init loop handle serverHandle)
                serverManager) $ \ _ -> withSockAddr serverAddr $ \ addrPtr -> do

                m <- getBlockMVar serverManager serverSlot
                acceptBuf <- newPinnedPrimArray (fromIntegral aCCEPT_BUFFER_SIZE)
                let acceptBufPtr = (coerce (mutablePrimArrayContents acceptBuf :: Ptr UVFD))

                withUVManager' serverManager $ do
                    throwUVIfMinus_ (uv_tcp_bind serverHandle addrPtr 0)
                    pokeBufferTable serverManager serverSlot acceptBufPtr 0
                    throwUVIfMinus_ (hs_uv_listen serverHandle (fromIntegral serverBackLog))

                forever $ do
                    takeMVar m

                    -- we lock uv manager here in case of next uv_run overwrite current accept buffer
                    acceptBufCopy <- withUVManager' serverManager $ do
                        tryTakeMVar m
                        accepted <- peekBufferTable serverManager serverSlot
                        acceptBuf' <- newPrimArray accepted
                        copyMutablePrimArray acceptBuf' 0 acceptBuf 0 accepted
                        pokeBufferTable serverManager serverSlot acceptBufPtr 0
                        unsafeFreezePrimArray acceptBuf'

                    let accepted = sizeofPrimArray acceptBufCopy

                    forM_ [0..accepted-1] $ \ i -> do
                        let fd = indexPrimArray acceptBufCopy i
                        if fd < 0
                        -- minus fd indicate a server error and we should close server
                        then throwUVIfMinus_ (return fd)
                        -- It's important to use the worker thread's mananger instead of server's one!
                        else void . forkBa $ do
                            uvm <- getUVManager
                            withResource (initTCPStream uvm) $ \ client -> do
                                withUVManager' (uvsManager client) $ do
                                    throwUVIfMinus_
                                        (hs_uv_tcp_open (uvsHandle client) fd)
                                    when serverWorkerNoDelay . throwUVIfMinus_ $
                                        uv_tcp_nodelay (uvsHandle client) 1
                                serverWorker client

                    when (accepted == fromIntegral aCCEPT_BUFFER_SIZE) $
                        withUVManager' serverManager $ uvListenResume serverHandle

    multipleAcceptLoop = do
        let (SocketFamily family) = sockAddrFamily serverAddr
        serverLock <- newEmptyMVar
        numCaps <- getNumCapabilities
        forM_ [0..numCaps-1] $ \ serverIndex -> forkOn serverIndex $ (`finally` tryPutMVar serverLock ()) $  do
            serverManager <- getUVManager
            withResource (initTCPExStream
                (fromIntegral $ family) serverManager) $ \ server -> do

                let serverHandle  = uvsHandle server
                serverSlot <- peekUVHandleData serverHandle

                withResource (initUVHandleNoSlot
                    uV_CHECK
                    (\ loop handle -> throwUVIfMinus_ $ hs_uv_accept_check_init loop handle serverHandle)
                    serverManager) $ \ _ -> withSockAddr serverAddr $ \ addrPtr -> do

                    m <- getBlockMVar serverManager serverSlot
                    acceptBuf <- newPinnedPrimArray (fromIntegral aCCEPT_BUFFER_SIZE)
                    let acceptBufPtr = (coerce (mutablePrimArrayContents acceptBuf :: Ptr UVFD))

                    withUVManager' serverManager $ do
                        throwUVIfMinus_ (hs_set_socket_reuse serverHandle)
                        throwUVIfMinus_ (uv_tcp_bind serverHandle addrPtr 0)
                        pokeBufferTable serverManager serverSlot acceptBufPtr 0
                        throwUVIfMinus_ (hs_uv_listen serverHandle (fromIntegral serverBackLog))

                    forever $ do
                        takeMVar m

                        -- we lock uv manager here in case of next uv_run overwrite current accept buffer
                        acceptBufCopy <- withUVManager' serverManager $ do
                            tryTakeMVar m
                            accepted <- peekBufferTable serverManager serverSlot
                            acceptBuf' <- newPrimArray accepted
                            copyMutablePrimArray acceptBuf' 0 acceptBuf 0 accepted
                            pokeBufferTable serverManager serverSlot acceptBufPtr 0
                            unsafeFreezePrimArray acceptBuf'

                        let accepted = sizeofPrimArray acceptBufCopy

                        forM_ [0..accepted-1] $ \ i -> do
                            let fd = indexPrimArray acceptBufCopy i
                            if fd < 0
                            -- minus fd indicate a server error and we should close server
                            then throwUVIfMinus_ (return fd)
                            -- Since the worker thread is on with same capability with server thread,
                            -- just use server's manager
                            else void . forkOn serverIndex  $ do
                                withResource (initTCPStream serverManager) $ \ client -> do
                                    withUVManager' (uvsManager client) $ do
                                        throwUVIfMinus_ (hs_uv_tcp_open (uvsHandle client) fd)
                                        when serverWorkerNoDelay . throwUVIfMinus_ $
                                            uv_tcp_nodelay (uvsHandle client) 1
                                    serverWorker client

                        when (accepted == fromIntegral aCCEPT_BUFFER_SIZE) $
                            withUVManager' serverManager $ uvListenResume serverHandle

        takeMVar serverLock

--------------------------------------------------------------------------------
