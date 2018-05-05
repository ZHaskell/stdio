{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : System.LowResTimer
Description : Low resolution (0.1s) timing wheel
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide low resolution (0.1s) timers using a timing wheel of size 128 per capability,
each timer thread will automatically started or stopped based on demannd. register or cancel a timeout is O(1),
and each step only need scan n/128 items given timers are registered in an even fashion.

This timer is particularly suitable for high concurrent approximated I/O timeout scheduling.
You should not rely on it to provide timing information since it's not very accurate.

Reference:

    * <https://github.com/netty/netty/blob/4.1/common/src/main/java/io/netty/util/HashedWheelTimer.java>
    * <http://www.cse.wustl.edu/~cdgill/courses/cs6874/TimingWheels.ppt>
-}


module System.LowResTimer
  ( -- * low resolution timers
    registerLowResTimer
  , registerLowResTimerOn
  , debounce
    -- * low resolution timer manager
  , LowResTimerManager
  , getLowResTimerManager
  , isLowResTimerManagerRunning
  , lowResTimerManagerCapabilitiesChanged
  ) where

import Data.Array
#ifndef mingw32_HOST_OS
import GHC.Event
#endif
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import GHC.Conc
import Data.IORef.Unboxed
import Data.IORef
import Data.Word
import qualified Control.Exception as E

--
queueSize :: Int
queueSize = 128

-- | A simple timing wheel
--
data TimerList = TimerItem {-# UNPACK #-} !Counter (IO ()) TimerList | TimerNil

data LowResTimerManager = LowResTimerManager
    { lrTimerQueue :: Array (IORef TimerList)
    , lrIndexLock :: MVar Int
    , lrRegisterCount :: Counter
    , lrRunningLock :: MVar Bool
    }

newLowResTimerManager :: IO LowResTimerManager
newLowResTimerManager = do
    indexLock <- newMVar 0
    regCounter <- newCounter 0
    runningLock <- newMVar False
    queue <- newArr queueSize
    forM [0..queueSize-1] $ \ i -> do
        writeArr queue i =<< newIORef TimerNil
    iqueue <- unsafeFreezeArr queue
    return (LowResTimerManager iqueue indexLock regCounter runningLock)

lowResTimerManager :: IORef (Array LowResTimerManager)
{-# NOINLINE lowResTimerManager #-}
lowResTimerManager = unsafePerformIO $ do
    numCaps <- getNumCapabilities
    lrtmArray <- newArr numCaps
    forM [0..numCaps-1] $ \ i -> do
        writeArr lrtmArray i =<< newLowResTimerManager
    ilrtmArray <- unsafeFreezeArr lrtmArray
    newIORef ilrtmArray

-- | Create new low resolution timer manager on capability change.
--
-- Since low resolution timer manager is not hooked into RTS, you're responsible to call this function
-- after you call 'setNumCapabilities' to match timer manager array size with new capability number.
--
-- This is not a must though, when we fetch timer manager we always take a modulo.
--
lowResTimerManagerCapabilitiesChanged :: IO ()
lowResTimerManagerCapabilitiesChanged = do
    lrtmArray <- readIORef lowResTimerManager
    let oldSize = sizeofArr lrtmArray
    numCaps <- getNumCapabilities
    when (numCaps /= oldSize) $ do
        lrtmArray' <- newArr numCaps
        if numCaps < oldSize
        then do
            forM [0..numCaps-1] $ \ i -> do
                writeArr lrtmArray' i =<< indexArrM lrtmArray i
        else do
            forM [0..oldSize-1] $ \ i -> do
                writeArr lrtmArray' i =<< indexArrM lrtmArray i
            forM [oldSize..numCaps-1] $ \ i -> do
                writeArr lrtmArray' i =<< newLowResTimerManager

        ilrtmArray' <- unsafeFreezeArr lrtmArray'
        atomicModifyIORef' lowResTimerManager $ \ _ -> (ilrtmArray', ())

-- | Get a 'LowResTimerManager' for current thread.
--
getLowResTimerManager :: IO LowResTimerManager
getLowResTimerManager = do
    (cap, _) <- threadCapability =<< myThreadId
    lrtmArray <- readIORef lowResTimerManager
    indexArrM lrtmArray (cap `rem` sizeofArr lrtmArray)

-- | Check if a timer manager's wheel is turning
--
-- This is mostly for testing purpose.
--
isLowResTimerManagerRunning :: LowResTimerManager -> IO Bool
isLowResTimerManagerRunning (LowResTimerManager _ _ _ runningLock) = readMVar runningLock

-- | Register a new timer on current capability's timer manager, start the timing wheel if it's not turning.
--
-- If the action could block, you may want to run it in another thread. Example to kill a thread after 10s:
--
-- @
--   registerLowResTimer 100 (forkIO $ killThread tid)
-- @
--
registerLowResTimer :: Int          -- ^ timout in unit of 100 milliseconds / 0.1s
                    -> IO ()        -- ^ the action you want to perform, it should not block
                    -> IO (IO ())   -- ^ cancel action
registerLowResTimer t action = do
    lrtm <- getLowResTimerManager
    registerLowResTimerOn lrtm t action

-- | Same as 'registerLowResTimer', but allow you choose timer manager.
--
registerLowResTimerOn :: LowResTimerManager   -- ^ a low resolution timer manager
                      -> Int          -- ^ timout in unit of 100 milliseconds / 0.1s
                      -> IO ()        -- ^ the action you want to perform, it should not block
                      -> IO (IO ())   -- ^ cancel action
registerLowResTimerOn lrtm@(LowResTimerManager queue indexLock regCounter _) t action = do

    let (round, tick) = (max 0 t) `quotRem` queueSize
    i <- readMVar indexLock
    tlistRef <- indexArrM queue ((i + tick) `rem` queueSize)
    roundCounter <- newCounter round
    E.mask_ $ do
        atomicModifyIORef' tlistRef $ \ tlist ->
            let newList = TimerItem roundCounter action tlist
            in (newList, ())
        atomicAddCounter regCounter 1

    ensureLowResTimerManager lrtm

    return (void $ atomicOrCounter roundCounter (-1))  -- cancel is simple, just set the round number to -1.
                                                       -- next scan will eventually release it

-- | Check if low resolution timer manager loop is running, start loop if not.
--
ensureLowResTimerManager :: LowResTimerManager -> IO ()
ensureLowResTimerManager lrtm@(LowResTimerManager _ _ _ runningLock) = do
    modifyMVar_ runningLock $ \ running -> do
        unless running $ do
            tid <- forkIO (startLowResTimerManager lrtm)
            labelThread tid "stdio: low resolution time manager"    -- make sure we can see it in GHC event log
        return True

-- | Start low resolution timer loop, the loop is automatically stopped if there's no more new registrations.
--
startLowResTimerManager :: LowResTimerManager ->IO ()
startLowResTimerManager lrtm@(LowResTimerManager _ _ regCounter runningLock)  = do
    modifyMVar_ runningLock $ \ _ -> do     -- we shouldn't receive async exception here
        c <- readIORefU regCounter          -- unless something terribly wrong happened, e.g., stackoverflow
        if c > 0
        then do
            forkIO (fireLowResTimerQueue lrtm)  -- we offload the scanning to another thread to minimize
                                                -- the time we holding runningLock
            case () of
                _
#ifndef mingw32_HOST_OS
                    | rtsSupportsBoundThreads -> do
                        htm <- getSystemTimerManager
                        void $ registerTimeout htm 100000 (startLowResTimerManager lrtm)
#endif
                    | otherwise -> void . forkIO $ do   -- we have to fork another thread since we're holding runningLock,
                        threadDelay 100000              -- this may affect accuracy, but on windows there're no other choices.
                        startLowResTimerManager lrtm
            return True
        else do
            return False -- if we haven't got any registered timeout, we stop the time manager
                         -- doing this can stop us from getting the way of idle GC
                         -- since we're still inside runningLock, we won't miss new registration.

-- | Scan the timeout queue in current tick index, and move tick index forward by one.
--
fireLowResTimerQueue :: LowResTimerManager -> IO ()
fireLowResTimerQueue lrtm@(LowResTimerManager queue indexLock regCounter runningLock) = do
    (tList, tListRef) <- modifyMVar indexLock $ \ index -> do                 -- get the index lock
        tListRef <- indexArrM queue index
        tList <- atomicModifyIORef' tListRef $ \ tList -> (TimerNil, tList)   -- swap current index list with an empty one
        let !index' = (index+1) `rem` queueSize                               -- move index forward by 1
        return (index', (tList, tListRef))                                    -- release the lock

    go tList tListRef regCounter
  where
    go (TimerItem roundCounter action nextList) tListRef regCounter = do
        r <- atomicSubCounter_ roundCounter 1
        case r `compare` 0 of
            LT -> do                                     -- if round number is less than 0, then it's a cancelled timer
                atomicSubCounter regCounter 1
                go nextList tListRef regCounter
            EQ -> do                                     -- if round number is equal to 0, fire it
                atomicSubCounter regCounter 1
                E.catch action ( \ (_ :: E.SomeException) -> return () )  -- well, we really don't want timers break our loop
                go nextList tListRef regCounter
            GT -> do                                     -- if round number is larger than 0, put it back for another round
                atomicModifyIORef' tListRef $ \ tlist -> (TimerItem roundCounter action tlist, ())
                go nextList tListRef regCounter
    go TimerNil _ _ = return ()

--------------------------------------------------------------------------------

-- | Cache result of an IO action for give time t.
--
-- This combinator is useful when you want to share IO result within a period, the action will be called
-- on demand, and the result will be cached for t milliseconds.
--
-- One common way to get a shared periodical updated value is to start a seperate thread,
-- but doing that will stop system from being idle, which stop idle GC from running,
-- and in turn disable deadlock detection, which is too bad. This function solves that.
--
debounce :: Int         -- ^ cache time in unit of 100 milliseconds / 0.1s
         -> IO a        -- ^ the original IO action
         -> IO (IO a)   -- ^ debounced IO action
debounce t action = do
    resultLock <- newEmptyMVar
    return $ do
        mresult <- tryReadMVar resultLock
        case mresult of
            Just result -> return result
            _           -> do
                result' <- action
                written <- tryPutMVar resultLock result'    -- there may be some contention here
                when written . void $             -- we don't have to success, but if we have
                    registerLowResTimer t         -- after t ms we clear result
                        (void $ tryTakeMVar resultLock)
                return result'
