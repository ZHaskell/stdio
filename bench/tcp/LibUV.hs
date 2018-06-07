{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO.TCP
import System.IO.Buffered
import Control.Concurrent
import Foreign.ForeignPtr
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr
import Control.Monad
import System.IO.Exception
import System.IO
import Data.IORef.Unboxed
import System.Environment
import Text.Read                        (readMaybe)

main :: IO ()
main = do
    portStr <- lookupEnv "PORT"
    reuseStr <- lookupEnv "REUSEPORT"
    let port = maybe 8888 id (readMaybe =<< portStr)
        reuse = maybe False id (readMaybe =<< reuseStr)
    let conf = defaultServerConfig{
            serverAddr = SockAddrInet port inetAny
        ,   serverWorker = \ uvs ->  do
                recvbuf <- mallocPlainForeignPtrBytes 2048  -- we reuse buffer as golang does,
                                                            -- since node use slab, which is in fact a memory pool
                                                            -- this is more fair

                -- do not print ECONNRESET for fairness
                catch (echo uvs recvbuf) (\ (e::SomeException) -> return ())
        ,   serverReusePortIfAvailable = reuse
        }

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




