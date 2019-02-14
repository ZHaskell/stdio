module Std.IO.LowResTimerSpec where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Std.Data.PrimIORef
import           Std.IO.LowResTimer
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = describe "low resolution timers" $ do
    it "timers registration should not be missed" $ do
        c <- newCounter 0
        replicateM_ 10000 $ do
            forM_ [1..10] $ \ i -> do
                registerLowResTimer i (atomicAddCounter_ c 1)

        threadDelay 1000
        lrtm <- getLowResTimerManager
        running <- isLowResTimerManagerRunning lrtm
        assertEqual "timer manager should start" True running

        threadDelay 1200000 -- make sure all timers are fired
        c' <- readPrimIORef c
        assertEqual "timers registration counter" 100000 c'

        threadDelay 100000  -- another 0.1s

        lrtm <- getLowResTimerManager
        running <- isLowResTimerManagerRunning lrtm
        assertEqual "timer manager should stopped" False running

    it "throttle" $ do
        c <- newCounter 0
        throttledAdd <- throttle 1 (atomicAddCounter_ c 1)
        forkIO . replicateM_ 10000 $ do
            throttledAdd
            threadDelay 500
        threadDelay 1000000  -- wait 1s here
        c' <- readPrimIORef c
        assertBool "throttled add" (6  <= c' && c' <= 7)

    it "throttleTrailing" $ do
        c <- newCounter 0
        throttledAdd <- throttleTrailing_ 1 (atomicAddCounter_ c 1)
        forkIO . replicateM_ 10000 $ do
            throttledAdd
            threadDelay 500
        threadDelay 1000000  -- wait 1s here
        c' <- readPrimIORef c
        assertBool "throttled add" (5  <= c' && c' <= 6)
