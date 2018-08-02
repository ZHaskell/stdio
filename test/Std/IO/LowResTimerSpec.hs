module Std.IO.LowResTimerSpec where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef.Unboxed
import           Std.IO.LowResTimer
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = describe "low resolution timers" $ do
    it "timers registration should not be missed" $ do
        c <- newCounter 0
        replicateM_ 10000 $ do
            forM_ [1..10] $ \ i -> do
                registerLowResTimer i (void $ atomicAddCounter c 1)

        threadDelay 1000
        lrtm <- getLowResTimerManager
        running <- isLowResTimerManagerRunning lrtm
        assertEqual "timer manager should start" True running

        threadDelay 1200000 -- make sure all timers are fired
        c' <- readIORefU c
        assertEqual "timers registration counter" 100000 c'

        threadDelay 100000  -- another 0.1s

        lrtm <- getLowResTimerManager
        running <- isLowResTimerManagerRunning lrtm
        assertEqual "timer manager should stopped" False running

    it "debounce sh" $ do
        c <- newCounter 0
        debouncedAdd <- debounce 1 (atomicAddCounter c 1)
        forkIO . replicateM_ 10000 $ do
            debouncedAdd
            threadDelay 500
        threadDelay 1000000  -- wait 1s here
        c' <- readIORefU c
        assertBool "debounced add" (5  <= c' && c' <= 6)
