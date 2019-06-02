{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Std.IO.LowResTimer
Description : Low resolution (0.1s) timing wheel
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide low resolution (0.1s) timers using a timing wheel of size 128 per capability,
each timer thread will automatically started or stopped based on demannd. register or cancel a timeout is O(1),
and each step only need scan n/128 items given timers are registered in an even fashion.

This timer is particularly suitable for high concurrent approximated IO timeout scheduling.
You should not rely on it to provide timing information since it's very inaccurate.

Reference:

    * <https://github.com/netty/netty/blob/4.1/common/src/main/java/io/netty/util/HashedWheelTimer.java>
    * <http://www.cse.wustl.edu/~cdgill/courses/cs6874/TimingWheels.ppt>
-}


module Std.IO.LowResTimer
  ( -- * low resolution timers
    registerLowResTimer
  , registerLowResTimer_
  , registerLowResTimerOn
  , LowResTimer
  , queryLowResTimer
  , cancelLowResTimer
  , cancelLowResTimer_
  , timeoutLowRes
  , timeoutLowResEx
  , throttle
  , throttle_
  , throttleTrailing_
    -- * low resolution timer manager
  , LowResTimerManager
  , getLowResTimerManager
  , isLowResTimerManagerRunning
  , lowResTimerManagerCapabilitiesChanged
  ) where

import           Std.Data.Array
#ifndef mingw32_HOST_OS
import           GHC.Event
#endif
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Std.IO.Exception
import           Control.Monad
import           Data.IORef
import           Std.Data.PrimIORef
import           Data.Word
import           GHC.Conc
import           System.IO.Unsafe

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
registerLowResTimer :: Int          -- ^ timeout in unit of 0.1s
                    -> IO ()        -- ^ the action you want to perform, it should not block
                    -> IO LowResTimer
registerLowResTimer t action = do
    lrtm <- getLowResTimerManager
    registerLowResTimerOn lrtm t action

-- | 'void' ('registerLowResTimer' t action)
registerLowResTimer_ :: Int          -- ^ timeout in unit of 0.1s
                     -> IO ()        -- ^ the action you want to perform, it should not block
                     -> IO ()
registerLowResTimer_ t action = void (registerLowResTimer t action)

-- | Same as 'registerLowResTimer', but allow you choose timer manager.
--
registerLowResTimerOn :: LowResTimerManager   -- ^ a low resolution timer manager
                      -> Int          -- ^ timeout in unit of 0.1s
                      -> IO ()        -- ^ the action you want to perform, it should not block
                      -> IO LowResTimer
registerLowResTimerOn lrtm@(LowResTimerManager queue indexLock regCounter _) t action = do

    let (round, tick) = (max 0 t) `quotRem` queueSize
    i <- readMVar indexLock
    tlistRef <- indexArrM queue ((i + tick) `rem` queueSize)
    roundCounter <- newCounter round
    mask_ $ do
        atomicModifyIORef' tlistRef $ \ tlist ->
            let newList = TimerItem roundCounter action tlist
            in (newList, ())
        atomicAddCounter_ regCounter 1

    ensureLowResTimerManager lrtm

    return (LowResTimer roundCounter)  -- cancel is simple, just set the round number to -1.
                                       -- next scan will eventually release it

-- | Timer registered by 'registerLowResTimer' or 'registerLowResTimerOn'.
--
newtype LowResTimer = LowResTimer Counter

-- | Query how many seconds remain before timer firing.
--
-- A return value <= 0 indictate the timer is firing or fired.
--
queryLowResTimer :: LowResTimer -> IO Int
queryLowResTimer (LowResTimer c) = readPrimIORef c

-- | Cancel a timer, return the remaining ticks.
--
-- This function have no effect after the timer is fired.
--
cancelLowResTimer :: LowResTimer -> IO Int
cancelLowResTimer (LowResTimer c) = atomicOrCounter c (-1)

-- | @void . cancelLowResTimer@
--
cancelLowResTimer_ :: LowResTimer -> IO ()
cancelLowResTimer_ = void . cancelLowResTimer

-- | similar to 'System.Timeout.timeout', this function put a limit on time which an IO can consume.
--
-- Note timeoutLowRes is also implemented with 'Exception' underhood, which can have some surprising
-- effects on some devices, e.g. use 'timeoutLowRes' with reading or writing on 'UVStream's will close
-- the 'UVStream' once a reading or writing is not able to be done in time.
timeoutLowRes :: Int    -- ^ timeout in unit of 0.1s
              -> IO a
              -> IO (Maybe a)
timeoutLowRes timeo io = do
    mid <- myThreadId
    catch
        (do timer <- registerLowResTimer timeo (timeoutAThread mid)
            r <- io
            cancelLowResTimer timer
            return (Just r))
        ( \ (e :: TimeOutException) -> return Nothing )
  where
    timeoutAThread id = void . forkIO $ throwTo id (TimeOutException id undefined)

-- | similar to 'timeoutLowRes', but raise a 'TimeOutException' instead of return 'Nothing'
-- if timeout.
timeoutLowResEx :: HasCallStack
                => Int    -- ^ timeout in unit of 0.1s
                -> IO a
                -> IO a
timeoutLowResEx timeo io = do
    mid <- myThreadId
    timer <- registerLowResTimer timeo (timeoutAThread mid)
    r <- io
    cancelLowResTimer timer
    return r
  where
    timeoutAThread id = void . forkIO $ throwTo id (TimeOutException id callStack)

data TimeOutException = TimeOutException ThreadId CallStack deriving Show
instance Exception TimeOutException

--------------------------------------------------------------------------------
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
        c <- readPrimIORef regCounter          -- unless something terribly wrong happened, e.g., stackoverflow
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
        r <- atomicSubCounter roundCounter 1
        case r `compare` 0 of
            LT -> do                                     -- if round number is less than 0, then it's a cancelled timer
                atomicSubCounter_ regCounter 1
                go nextList tListRef regCounter
            EQ -> do                                     -- if round number is equal to 0, fire it
                atomicSubCounter_ regCounter 1
                catch action ( \ (_ :: SomeException) -> return () )  -- well, we really don't want timers break our loop
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
-- One common way to get a shared periodical updated value is to start a seperate thread and do calculation
-- periodically, but doing that will stop system from being idle, which stop idle GC from running,
-- and in turn disable deadlock detection, which is too bad. This function solves that.
throttle :: Int         -- ^ cache time in unit of 0.1s
         -> IO a        -- ^ the original IO action
         -> IO (IO a)   -- ^ throttled IO action
throttle t action = do
    resultCounter <- newCounter 0
    resultRef <- newIORef =<< action
    return $ do
        c <- atomicOrCounter resultCounter (-1) -- 0x11111111 or 0x1111111111111111 depend machine word size
        if c == 0
        then do
            registerLowResTimer_ t (void $ atomicAndCounter resultCounter 0)
            !r <- action
            atomicWriteIORef resultRef r
            return r
        else readIORef resultRef

-- | Throttle an IO action without caching result.
--
-- The IO action will run at leading edge. i.e. once run, during following (t/10)s throttled action will
-- no-ops.
--
-- Note the action will run in the calling thread.
throttle_ :: Int            -- ^ cache time in unit of 0.1s
          -> IO ()          -- ^ the original IO action
          -> IO (IO ())     -- ^ throttled IO action
throttle_ t action = do
    resultCounter <- newCounter 0
    return $ do
        c <- atomicOrCounter resultCounter (-1) -- 0x11111111 or 0x1111111111111111 depend machine word size
        when (c == 0) $ do
            registerLowResTimer_ t (void $ atomicAndCounter resultCounter 0)
            void action

-- | Similar to 'throttle_' but run action in trailing edge
--
-- The IO action will run at trailing edge. i.e. no matter how many times throttled action
-- are called, original action will run only once after (t/10)s.
--
-- Note the action will be run in a new created thread.
throttleTrailing_ :: Int
                  -> IO ()        -- ^ the original IO action
                  -> IO (IO ())   -- ^ throttled IO action
throttleTrailing_ t action = do
    resultCounter <- newCounter 0
    return $ do
        c <- atomicOrCounter resultCounter (-1) -- 0x11111111 or 0x1111111111111111 depend machine word size
        when (c == 0) . registerLowResTimer_ t . void . forkIO $ do
            atomicAndCounter_ resultCounter 0
            action
