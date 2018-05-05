{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO.Net
import System.IO.Buffered
import Control.Concurrent
import Foreign.ForeignPtr
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr
import Control.Monad
import System.IO.Exception
import System.IO.UV.Stream
import System.IO
import Data.IORef.Unboxed
import System.Environment
import Text.Read                        (readMaybe)

main :: IO ()
main = do
    portStr <- lookupEnv "PORT"
    let port = maybe 8888 id (readMaybe =<< portStr)
    let conf = ServerConfig
            (SockAddrInet port inetAny)
            128
            (\ uvs -> do
                recvbuf <- mallocPlainForeignPtrBytes 2048  -- we reuse buffer as golang does,
                                                            -- since node use slab, which is in face a memory pool
                echo uvs recvbuf)
            True
            (print :: SomeException -> IO())

    startServer conf
  where
    echo uvs recvbuf = loop
      where
        loop = do
            r <- withForeignPtr recvbuf $ \ p -> do
                readInput uvs p 2048
            when (r /= 0) $ do
                withForeignPtr sendbuffp $ \ p -> writeOutput uvs p l
                loop

    (B.PS sendbuffp _ l) =
        "HTTP/1.1 200 OK\r\n\
        \Content-Type: text/html; charset=UTF-8\r\n\
        \Content-Length: 500\r\n\
        \Connection: Keep-Alive\r\n\
        \\r\n" `B.append` (B.replicate 500 48)




