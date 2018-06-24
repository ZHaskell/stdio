{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unit.TCP where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Typeable
import qualified Data.Vector as V
import Data.IORef.Unboxed
import System.IO.Resource as R
import System.IO.TCP

data WorkerException = WorkerException deriving (Typeable, Show)

instance Exception WorkerException

unitResource :: TestTree
unitResource = testGroup "tcp" [
        testCase "server loopback" $ do
            forkIO $ startServer defaultServerConfig {
                    serverWorker = \ uvs -> do
                        i <- newBufferedInput uvs 4096
                        withResource (initBufferedOutput uvs 0) $ \ o ->
                            forever $ do
                                b <- readBuffer i
                                when (V.length b /= 0) (writeBytes o b)
                }
            R.withResource (initClient defaultClientConfig) $ \ c ->
                i <- newBufferedInput uvs 4096
                withResource (initBufferedOutput uvs 0) $ \ o ->
                    (writeBytes o b [])

                -- assertEqual "pool should stop scanning returned resources" PoolEmpty s

    ]
