{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.IO.ResourceSpec where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Std.Data.PrimIORef
import           Data.Typeable
import           Std.IO.Resource          as R
import           Test.Hspec
import           Test.HUnit

data WorkerException = WorkerException deriving (Typeable, Show)

instance Exception WorkerException

spec :: Spec
spec = describe "resource tests" $ do
    it "resource pool" $ do
        resCounter <- newCounter 0
        workerCounter <- newCounter 0
        let res = initResource (atomicAddCounter_ resCounter 1)
                               (\ _ -> atomicSubCounter_ resCounter 1)
            resPool = initPool res 100 1
        R.withResource resPool $ \ pool -> do
            let res = initInPool pool
            replicateM_ 300 . forkIO. R.withResource res $ \ _ -> do
                atomicAddCounter_ workerCounter 1
                r <- readPrimIORef resCounter
                threadDelay 1000000
                assertEqual "pool should limit max usage" True (r <= 100)

            threadDelay 4000000 -- first 100 worker quickly get resources
                                -- then hold for 1s, rest 100 worker have to wait, and so on
                                -- so here we wait for 4s to make sure every worker got a resource
                                -- we used to use replicateConcurrently_ from async, but it's
                                -- not really neccessary

            w <- readPrimIORef workerCounter
            assertEqual "worker should be able to get resource" 300 w

            r <- readPrimIORef resCounter
            assertEqual "pool should keep returned resources alive" 100 r

            s <- statPool pool
            assertEqual "pool should be scanning returned resources" PoolScanning s

            threadDelay 1200000  -- another 1.2s

            r <- readPrimIORef resCounter
            assertEqual "pool should reap unused resources" 0 r

            threadDelay 1200000  -- another 1.2s

            s <- statPool pool
            assertEqual "pool should stop scanning returned resources" PoolEmpty s

            -- Let's test again

            writePrimIORef workerCounter 0

            replicateM_ 300 . forkIO. R.withResource res $ \ _ -> do
                atomicAddCounter_ workerCounter 1
                r <- readPrimIORef resCounter
                threadDelay 1000000
                assertEqual "pool should limit max usage" True (r <= 100)

            threadDelay 4000000

            w <- readPrimIORef workerCounter
            assertEqual "worker should be able to get resource" 300 w

            r <- readPrimIORef resCounter
            assertEqual "pool should keep returned resources alive" 100 r

            s <- statPool pool
            assertEqual "pool should be scanning returned resources" PoolScanning s

            threadDelay 1200000  -- another 1.2s

            r <- readPrimIORef resCounter
            assertEqual "pool should reap unused resources" 0 r

            threadDelay 1200000  -- another 1.2s

            s <- statPool pool
            assertEqual "pool should stop scanning returned resources" PoolEmpty s

    it "resource pool under exceptions" $ do
        resCounter <- newCounter 0
        let res = initResource (atomicAddCounter' resCounter 1)
                               (\ _ -> atomicSubCounter_ resCounter 1)
            resPool = initPool res 100 1
        R.withResource resPool $ \ pool -> do
            let res = initInPool pool
            handle (\ (e :: WorkerException) -> return ()) .
                    replicateM_ 300 . forkIO. R.withResource res $ \ i -> do
                        r <- readPrimIORef resCounter
                        threadDelay 1000000
                        when (even i) (throwIO WorkerException)
                        assertEqual "pool should limit max usage" True (r <= 100)

            threadDelay 4000000

            r <- readPrimIORef resCounter
            assertEqual "pool should keep returned resources alive" 100 r

            s <- statPool pool
            assertEqual "pool should be scanning returned resources" PoolScanning s

            threadDelay 1200000  -- another 1.2s

            r <- readPrimIORef resCounter
            assertEqual "pool should reap unused resources" 0 r

            threadDelay 1200000  -- another 1.2s

            s <- statPool pool
            assertEqual "pool should stop scanning returned resources" PoolEmpty s
