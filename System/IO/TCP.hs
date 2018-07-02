{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

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
import Foreign.C.Types

initTCPStream :: HasCallStack => UVManager -> Resource UVStream
initTCPStream = initUVStream (\ loop handle ->
    throwUVIfMinus_ (uv_tcp_init loop handle))

initTCPExStream :: HasCallStack => CUInt -> UVManager -> Resource UVStream
initTCPExStream family = initUVStream (\ loop handle ->
    throwUVIfMinus_ (uv_tcp_init_ex loop handle family))

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
    let handle = uvsHandle client
    liftIO . withSockAddr clientTargetAddr $ \ targetPtr -> do
        forM_ clientLocalAddr $ \ clientLocalAddr' ->
            withSockAddr clientLocalAddr' $ \ localPtr ->
                -- safe without withUVManager
                throwUVIfMinus_ (uv_tcp_bind handle localPtr 0)
        -- safe without withUVManager
        when clientNoDelay $ throwUVIfMinus_ (uv_tcp_nodelay handle 1)
        withUVManagerWrap uvm $ \ _ -> hs_uv_tcp_connect handle targetPtr
    return client

--------------------------------------------------------------------------------

-- | A TCP server configuration
--
data ServerConfig = ServerConfig
    { serverAddr       :: SockAddr
    , serverBackLog    :: Int
    , serverWorker     :: UVStream -> IO ()
    , serverWorkerNoDelay :: Bool
    }

-- | A default hello world server on localhost:8888
--
-- Test it with @main = startServer defaultServerConfig@, now try @nc -v 127.0.0.1 8888@
--
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
    (SockAddrInet 8888 inetAny)
    128
    (\ uvs -> writeOutput uvs (Ptr "hello world"#) 11)
    True

-- | Start a server
--
-- Fork new worker thread upon a new connection.
--
startServer :: ServerConfig -> IO ()
startServer ServerConfig{..} = do
    serverManager <- getUVManager
    withResource (initTCPStream serverManager) $ \ (UVStream serverHandle serverSlot _ _) ->
        bracket
            (throwOOMIfNull $ hs_uv_accept_check_alloc serverHandle)
            (hs_uv_accept_check_close) $ \ check -> do
                throwUVIfMinus_ $ hs_uv_accept_check_init check
                withSockAddr serverAddr $ \ addrPtr -> do
                    m <- getBlockMVar serverManager serverSlot
                    acceptBuf <- newPinnedPrimArray (fromIntegral aCCEPT_BUFFER_SIZE)
                    let acceptBufPtr = (coerce (mutablePrimArrayContents acceptBuf :: Ptr UVFD))

                    withUVManager_ serverManager $ do
                        pokeBufferTable serverManager serverSlot acceptBufPtr 0
                        throwUVIfMinus_ (uv_tcp_bind serverHandle addrPtr 0)
                        throwUVIfMinus_ (hs_uv_listen serverHandle (fromIntegral serverBackLog))

                    forever $ do
                        takeMVar m

                        -- we lock uv manager here in case of next uv_run overwrite current accept buffer
                        acceptBufCopy <- withUVManager_ serverManager $ do
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
                                withResource (initUVStream (\ loop handle -> do
                                    throwUVIfMinus_ (uv_tcp_init loop handle)
                                    throwUVIfMinus_ (hs_uv_tcp_open handle fd)) uvm) $ \ client -> do
                                    when serverWorkerNoDelay . throwUVIfMinus_ $
                                        -- safe without withUVManager
                                        uv_tcp_nodelay (uvsHandle client) 1
                                    serverWorker client

                        when (accepted == fromIntegral aCCEPT_BUFFER_SIZE) $
                            withUVManager_ serverManager (hs_uv_listen_resume serverHandle)
