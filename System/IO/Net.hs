{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|
Module      : System.IO.Net
Description : TCP or IPC servers and clients
Copyright   : (c) Winterland, 2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provides an API for creating TCP or IPC servers and clients. IPC Support is implemented with named pipes on Windows, and UNIX domain sockets on other operating systems.

On UNIX, the local domain is also known as the UNIX domain. The path is a filesystem path name. It gets truncated to sizeof(sockaddr_un.sun_path) - 1, which varies on different operating system between 91 and 107 bytes. The typical values are 107 on Linux and 103 on macOS. The path is subject to the same naming conventions and permissions checks as would be done on file creation. It will be visible in the filesystem, and will persist until unlinked.

On Windows, the local domain is implemented using a named pipe. The path must refer to an entry in \\?\pipe\ or \\.\pipe\. Any characters are permitted, but the latter may do some processing of pipe names, such as resolving .. sequences. Despite appearances, the pipe name space is flat. Pipes will not persist, they are removed when the last reference to them is closed. Do not forget JavaScript string escaping requires paths to be specified with double-backslashes, such as:

net.createServer().listen(
  path.join('\\\\?\\pipe', process.cwd(), 'myctl'));

-}

module System.IO.Net (
    initTCPConnection
  , ServerConfig(..)
  , defaultServerConfig
  , startServer
  , module System.IO.Net.SockAddr
  ) where


import System.IO.Net.SockAddr
import System.IO.Exception
import System.IO.Buffered
import System.IO.UV.Manager
import System.IO.UV.Stream
import System.IO.UV.Internal
import Control.Concurrent.MVar
import Foreign.Ptr
import GHC.Ptr
import Foreign.C.Types (CInt(..), CSize(..))
import Data.Int
import Data.Vector
import Data.IORef.Unboxed
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Foreign.PrimArray

initTCPConnection :: HasCallStack
        => SockAddr
        -> Maybe SockAddr
        -> Resource UVStream
initTCPConnection target local = do
    conn <- initTCPStream
    let uvm = uvsManager conn
        handle = uvsHandle conn
    connSlot <- initUVSlot uvm
    connReq <- initUVReq uV_CONNECT
    liftIO $ do
        forM_ local $ \ local' -> withSockAddr local' $ \ localPtr ->
            uvTCPBind handle localPtr False

        withSockAddr target $ \ target' -> do
            m <- getBlockMVar uvm connSlot
            tryTakeMVar m
            pokeUVReqData connReq connSlot
            withUVManager' uvm $ uvTCPConnect connReq handle target'
            takeMVar m
            throwUVIfMinus_ $ peekBufferTable uvm connSlot
    return conn

-- | A TCP/Pipe server configuration
--
data ServerConfig = forall e. Exception e => ServerConfig
    { serverAddr       :: SockAddr
    , serverBackLog    :: Int
    , serverWorker     :: UVStream -> IO ()
    , serverWorkerNoDelay :: Bool
    , serverWorkerHandler :: e -> IO ()
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
    (print :: SomeException -> IO())

-- | Start a server
--
-- Fork new worker thread upon a new connection.
--
startServer :: ServerConfig -> IO ()
startServer ServerConfig{..} =
    withResource initTCPStream $ \ server -> do

    let serverHandle = uvsHandle server
        serverManager = uvsManager server
        serverSlot = uvsReadSlot server

    withResource (initUVHandle uV_CHECK
        (\ loop handle -> hs_uv_accept_check_init loop handle serverHandle >> return handle) serverManager) $ \ _ ->
            withSockAddr serverAddr $ \ addrPtr -> do

        m <- getBlockMVar serverManager serverSlot
        acceptBuf <- newPinnedPrimArray (fromIntegral aCCEPT_BUFFER_SIZE)
        let acceptBufPtr = (coerce (mutablePrimArrayContents acceptBuf :: Ptr UVFD))

        withUVManager' serverManager $ do
            tryTakeMVar m
            uvTCPBind serverHandle addrPtr False
            pokeBufferTable serverManager serverSlot acceptBufPtr 0
            uvListen serverHandle (fromIntegral serverBackLog)

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
                then throwUVIfMinus_ (return fd)    -- minus fd indicate a server error and we should close server
                else do
                    void . forkBa . withResource initTCPStream $ \ client -> do
                        handle serverWorkerHandler $ do
                            withUVManager' (uvsManager client) $ do
                                uvTCPOpen (uvsHandle client) (fromIntegral fd)
                                when serverWorkerNoDelay $
                                    uvTCPNodelay (uvsHandle client) True
                            serverWorker client

            when (accepted == fromIntegral aCCEPT_BUFFER_SIZE) $
                withUVManager' serverManager $ uvListenResume serverHandle

--------------------------------------------------------------------------------

uvListen :: HasCallStack => Ptr UVHandle -> CInt -> IO ()
uvListen handle backlog = throwUVIfMinus_ (hs_uv_listen handle backlog)
foreign import ccall unsafe hs_uv_listen  :: Ptr UVHandle -> CInt -> IO CInt

foreign import ccall unsafe hs_uv_accept_check_init  :: Ptr UVLoop -> Ptr UVHandle -> Ptr UVHandle -> IO CInt

foreign import ccall unsafe "hs_uv_listen_resume" uvListenResume :: Ptr UVHandle -> IO ()
