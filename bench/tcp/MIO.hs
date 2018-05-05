{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import GHC.ForeignPtr
import Foreign.ForeignPtr
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import Control.Concurrent.MVar
import Control.Exception
import Data.IORef.Unboxed
import System.Environment
import Text.Read                        (readMaybe)

main :: IO ()
main = do
    portStr <- lookupEnv "PORT"
    let port = maybe 8888 id (readMaybe =<< portStr)
    sock <- socket AF_INET Stream defaultProtocol
    bind sock $ SockAddrInet port iNADDR_ANY
    listen sock 128
    cap <- getNumCapabilities
    capCounter <- newCounter 0
    onException (forever $ do
        (sock' , addr) <- accept sock
        c <- atomicAddCounter_ capCounter 1
        forkOn c $ do
            setSocketOption sock' NoDelay 1
            recvbuf <- mallocPlainForeignPtrBytes 2048  -- we reuse buffer as golang does,
                                                        -- since node use slab, which is in face a memory pool
            echo sock' recvbuf)
        (close sock)

  where
    echo sock recvbuf = loop >> close sock
      where
        loop = do
            r <- withForeignPtr recvbuf $ \ p -> do
                recvBuf sock p 2048

            when (r /= 0) $ do
                sendAll sock sendbuf
                loop

    sendbuf =
        "HTTP/1.1 200 OK\r\n\
        \Content-Type: text/html; charset=UTF-8\r\n\
        \Content-Length: 500\r\n\
        \Connection: Keep-Alive\r\n\
        \\r\n" `B.append` (B.replicate 500 48)

