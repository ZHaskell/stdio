module Unit.Resource where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad
import Data.IORef.Unboxed
import System.IO.Resource as R

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
                replicateConcurrently_ 1000 . R.withResource res $ \ _ -> do
                    atomicAddCounter workerCounter 1
                    r <- readIORefU resCounter
                    assertEqual "pool should limit max usage" True (r <= 100)
                    threadDelay 1000000

                r <- readIORefU resCounter
                assertEqual "pool should keep returned resources alive" 100 r

                w <- readIORefU workerCounter
                assertEqual "worker should be able to get resource" True (w == 1000)

                threadDelay 2000000  -- another 2s

                r <- readIORefU resCounter
                assertEqual "pool should reap unused resources" 0 r

    ]
