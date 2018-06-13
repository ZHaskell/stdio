{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unit.Resource where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Typeable
import Data.IORef.Unboxed
import System.IO.Resource as R

data WorkerException = WorkerException deriving (Typeable, Show)

instance Exception WorkerException

unitResource :: TestTree
unitResource = testGroup "resource" [
        testCase "resource pool" $ do
            resCounter <- newCounter 0
            workerCounter <- newCounter 0
            let res = initResource (atomicAddCounter resCounter 1)
                                   (\ _ -> void $ atomicSubCounter resCounter 1)
                resPool = initPool res 100 1
            R.withResource resPool $ \ pool -> do
                let res = usePool pool
                replicateConcurrently_ 300 . R.withResource res $ \ _ -> do
                    atomicAddCounter workerCounter 1
                    r <- readIORefU resCounter
                    threadDelay 1000000
                    assertEqual "pool should limit max usage" True (r <= 100)

                w <- readIORefU workerCounter
                assertEqual "worker should be able to get resource" 300 w

                r <- readIORefU resCounter
                assertEqual "pool should keep returned resources alive" 100 r

                s <- statPool pool
                assertEqual "pool should be scanning returned resources" PoolScanning s

                threadDelay 1200000  -- another 1.2s

                r <- readIORefU resCounter
                assertEqual "pool should reap unused resources" 0 r

                threadDelay 1200000  -- another 1.2s

                s <- statPool pool
                assertEqual "pool should stop scanning returned resources" PoolEmpty s

                -- Let's test again

                writeIORefU workerCounter 0

                replicateConcurrently_ 300 . R.withResource res $ \ _ -> do
                    atomicAddCounter workerCounter 1
                    r <- readIORefU resCounter
                    threadDelay 1000000
                    assertEqual "pool should limit max usage" True (r <= 100)

                w <- readIORefU workerCounter
                assertEqual "worker should be able to get resource" 300 w

                r <- readIORefU resCounter
                assertEqual "pool should keep returned resources alive" 100 r

                s <- statPool pool
                assertEqual "pool should be scanning returned resources" PoolScanning s

                threadDelay 1200000  -- another 1.2s

                r <- readIORefU resCounter
                assertEqual "pool should reap unused resources" 0 r

                threadDelay 1200000  -- another 1.2s

                s <- statPool pool
                assertEqual "pool should stop scanning returned resources" PoolEmpty s

    ,
        testCase "resource pool under exceptions" $ do
            resCounter <- newCounter 0
            let res = initResource (atomicAddCounter resCounter 1)
                                   (\ _ -> void $ atomicSubCounter resCounter 1)
                resPool = initPool res 100 1
            R.withResource resPool $ \ pool -> do
                let res = usePool pool
                handle (\ (e :: WorkerException) -> return ()) .
                        replicateConcurrently_ 300 . R.withResource res $ \ i -> do
                    r <- readIORefU resCounter
                    threadDelay 1000000
                    when (even i) (throwIO WorkerException)
                    assertEqual "pool should limit max usage" True (r <= 100)

                r <- readIORefU resCounter
                assertEqual "pool should keep returned resources alive" 100 r

                s <- statPool pool
                assertEqual "pool should be scanning returned resources" PoolScanning s

                threadDelay 1200000  -- another 1.2s

                r <- readIORefU resCounter
                assertEqual "pool should reap unused resources" 0 r

                threadDelay 1200000  -- another 1.2s

                s <- statPool pool
                assertEqual "pool should stop scanning returned resources" PoolEmpty s
    ]
