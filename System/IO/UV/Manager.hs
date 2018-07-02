{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

{-|
Module      : System.IO.UV.Manager
Description : I/O manager based on libuv
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide I/O manager which bridge libuv's async interface with ghc's light weight thread.

The main procedures for doing event I/O is:

+ Allocate a slot number using 'allocSlot'.
+ Prepare you I/O buffer and write them to uv loop with 'pokeBufferTable'(both read and write).
+ Block your thread with a 'MVar', using 'getBlockMVar' to get it.
+ Read the result with 'getResult', for read it's the read bytes number, for write it will be zero.
  Use 'E.throwIfError' to guard error situations.
+ Return the slot back uv manager with 'freeSlot'.

Usually slots are cache in the I/O device so that you don't have to allocate new one before each I/O operation.
Check "System.IO.Socket.TCP" as an example.

-}

module System.IO.UV.Manager
  ( UVManager
  , getUVManager
  , getBlockMVar
  , peekBufferTable
  , pokeBufferTable
  , withUVManager
  , withUVManager_
  , withUVManagerWrap
  , withUVManagerWrap_
  -- * handle/request resources
  , initUVStream
  , UVStream(..)
  -- * concurrent helpers
  , forkBa
  ) where

import GHC.Stack.Compat
import GHC.Conc.Sync (labelThread)
import Data.Array
import Data.Primitive.PrimArray
import Data.Word
import Data.IORef
import Data.IORef.Unboxed
import Foreign hiding (void, with)
import Foreign.C
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Concurrent.QSemN
import Control.Monad
import Control.Monad.IO.Class
import Data.Primitive.Addr
import System.IO.Unsafe
import System.IO.Exception
import System.IO.UV.Exception
import System.IO.UV.Internal
import System.IO.Buffered
import System.IO.Resource

#define IDLE_LIMIT 20

--------------------------------------------------------------------------------

data UVManager = UVManager
    { uvmBlockTable   :: {-# UNPACK #-} !(IORef (UnliftedArray (MVar Int))) -- a array to store threads blocked on async I/O.

    , uvmLoop        :: {-# UNPACK #-} !(Ptr UVLoop)        -- the uv loop refrerence

    , uvmLoopData    :: {-# UNPACK #-} !(Ptr UVLoopData)    -- cached pointer to uv_loop_t's data field

    , uvmRunning     :: {-# UNPACK #-} !(MVar Bool)     -- only uv manager thread will modify this value.
                                                        -- 'True' druing uv_run and 'False' otherwise.
                                                        --
                                                        -- unlike epoll/ONESHOT, uv loop are NOT thread safe,
                                                        -- we have to wake up the loop before mutating uv_loop's
                                                        -- state.
    , uvmCap ::  {-# UNPACK #-} !Int                -- the capability uv manager run on.
    }

instance Show UVManager where
    show uvm = "UVManager on capability " ++ show (uvmCap uvm)

instance Eq UVManager where
    uvm == uvm' =
        uvmCap uvm == uvmCap uvm'

uvManagerArray :: IORef (Array UVManager)
{-# NOINLINE uvManagerArray #-}
uvManagerArray = unsafePerformIO $ do
    numCaps <- getNumCapabilities
    uvmArray <- newArr numCaps
    s <- newQSemN 0
    forM_ [0..numCaps-1] $ \ i -> do
        -- fork uv manager thread
        forkOn i . withResource (initUVManager iNIT_LOOP_SIZE i) $ \ m -> do
            myThreadId >>= (`labelThread` ("uv manager on " ++ show i))
            writeArr uvmArray i m
            signalQSemN s 1
            startUVManager m
    waitQSemN s numCaps
    iuvmArray <- unsafeFreezeArr uvmArray
    newIORef iuvmArray

-- | Get 'UVManager' runing on the same capability.
--
getUVManager :: IO UVManager
{-# INLINABLE getUVManager #-}
getUVManager = do
    (cap, _) <- threadCapability =<< myThreadId
    uvmArray <- readIORef uvManagerArray
    indexArrM uvmArray (cap `rem` sizeofArr uvmArray)

-- | Get 'MVar' from blocking table with given slot.
--
getBlockMVar :: UVManager -> UVSlot -> IO (MVar Int)
{-# INLINABLE getBlockMVar #-}
getBlockMVar uvm slot = do
    blockTable <- readIORef (uvmBlockTable uvm)
    indexArrM blockTable slot

-- | Poke a prepared buffer and size into loop data under given slot.
--
-- NOTE, this action is not protected with 'withUVManager_ for effcient reason, you should merge this action
-- with other uv action and put them together inside a 'withUVManager_ or 'withUVManager\''. for example:
--
--  @@@
--      ...
--      withUVManager_ uvm $ do
--          pokeBufferTable uvm slot buf len
--          uvReadStart handle
--      ...
--  @@@
--
pokeBufferTable :: UVManager -> UVSlot -> Ptr Word8 -> Int -> IO ()
{-# INLINABLE pokeBufferTable #-}
pokeBufferTable uvm slot buf bufSiz = do
    (bufTable, bufSizTable) <- peekUVBufferTable (uvmLoopData uvm)
    pokeElemOff bufTable slot buf
    pokeElemOff bufSizTable slot (fromIntegral bufSiz)

peekBufferTable :: UVManager -> UVSlot -> IO Int
{-# INLINABLE peekBufferTable #-}
peekBufferTable uvm slot = do
    (bufTable, bufSizTable) <- peekUVBufferTable (uvmLoopData uvm)
    fromIntegral <$> peekElemOff bufSizTable slot

initUVManager :: HasCallStack => Int -> Int -> Resource UVManager
initUVManager siz cap = do
    loop  <- initUVLoop (fromIntegral siz)
    liftIO $ do
        mblockTable <- newArr siz
        forM_ [0..siz-1] $ \ i -> writeArr mblockTable i =<< newEmptyMVar
        blockTable <- unsafeFreezeArr mblockTable
        blockTableRef <- newIORef blockTable
        loopData <- peekUVLoopData loop
        running <- newMVar False
        return (UVManager blockTableRef loop loopData running cap)
  where
    initUVLoop :: HasCallStack => Int -> Resource (Ptr UVLoop)
    initUVLoop siz = initResource
        (throwOOMIfNull $ hs_uv_loop_init siz
        ) hs_uv_loop_close

-- | Lock an uv mananger, so that we can safely mutate its uv_loop's state.
--
-- libuv is not thread safe, use this function to perform any action which will mutate uv_loop's state.
--
withUVManager :: HasCallStack => UVManager -> (Ptr UVLoop -> IO a) -> IO a
withUVManager (UVManager _ loop loopData running _) f = go
  where
    go = do
        r <- withMVar running $ \ running ->
            if running
            then do
                -- if uv_run is running, it will stop
                -- if uv_run is not running, next running won't block
                throwUVIfMinus_ (hs_uv_wake_up_async loopData)
                return Nothing
            else do
                r <- f loop
                return (Just r)
        case r of
            Just r' -> return r'
            _       -> yield >> go -- we yield here, because uv_run is probably not finished yet

-- | Lock an uv mananger, so that we can safely mutate its uv_loop's state.
--
-- Some action did not request uv_loop pointer explicitly, but will mutate uv_loop underhood, for example:
-- @uv_read_start@. These actions have to be protected by locking the uv_loop.
--
-- In fact most of the libuv's functions are not thread safe, so watch out!
--
withUVManager_ :: HasCallStack => UVManager -> IO a -> IO a
withUVManager_ uvm f = withUVManager uvm (\ _ -> f)

-- | Start the uv loop
--
startUVManager :: HasCallStack => UVManager -> IO ()
startUVManager uvm@(UVManager _ _ _ running _) = loop -- use a closure capture uvm in case of stack memory leaking
  where
    loop = do
        e <- withMVar running $ \ _ -> step uvm False   -- we borrow mio's non-blocking/blocking poll strategy here
        if e > 0                                        -- first we do a non-blocking poll, if we got events
        then yield >> loop                              -- we yield here, to let other threads do actual work
        else do                                         -- otherwise we still yield once
            yield                                       -- in case other threads can still progress
            e <- withMVar running $ \ _ -> step uvm False   -- now we do another non-blocking poll to make sure
            if e > 0 then yield >> loop             -- if we got events somehow, we yield and go back
            else do                                 -- if there's still no events, we directly jump to safe blocking poll
                _ <- swapMVar running True          -- after swap this lock, other thread can wake up us
                e <- step uvm True                  -- by send async handler, and it's thread safe
                _ <- swapMVar running False

                yield                               -- we yield here, to let other threads do actual work
                loop

    -- call uv_run, return the event number
    step :: UVManager -> Bool -> IO Int
    step (UVManager blockTableRef loop loopData _ _) block = do
            blockTable <- readIORef blockTableRef
            clearUVEventCounter loopData        -- clean event counter

            if block
            then if rtsSupportsBoundThreads
                then throwUVIfMinus_ $ uv_run_safe loop uV_RUN_ONCE
                else do
                    -- use a 1ms timeout blocking poll on non-threaded rts
                    throwUVIfMinus_ (hs_uv_wake_up_timer loopData)
                    throwUVIfMinus_ (uv_run loop uV_RUN_ONCE)
            else throwUVIfMinus_ (uv_run loop uV_RUN_NOWAIT)

            (c, q) <- peekUVEventQueue loopData
            forM_ [0..c-1] $ \ i -> do
                slot <- peekElemOff q i
                lock <- indexArrM blockTable slot
                -- unlock ghc thread with result
                r <- peekBufferTable uvm slot
                tryPutMVar lock r
            return c

getUVSlot :: HasCallStack => UVManager -> IO UVSlotUnSafe -> IO UVSlot
{-# INLINE getUVSlot #-}
getUVSlot (UVManager blockTableRef _ _ _ _) f = do
    slot <- throwUVIfMinus (unsafeGetSlot <$> f)
    blockTable <- readIORef blockTableRef
    let oldSiz = sizeofArr blockTable
    when (slot == oldSiz) $ do
        let newSiz = oldSiz `shiftL` 2
        blockTable' <- newArr newSiz
        copyArr blockTable' 0 blockTable 0 oldSiz
        forM_ [oldSiz..newSiz-1] $ \ i ->
            writeArr blockTable' i =<< newEmptyMVar
        !iBlockTable' <- unsafeFreezeArr blockTable'
        writeIORef blockTableRef iBlockTable'
    return slot

--------------------------------------------------------------------------------

data UVStream = UVStream
    { uvsHandle :: {-# UNPACK #-} !(Ptr UVHandle)
    , uvsSlot    :: {-# UNPACK #-} !UVSlot
    , uvsManager :: UVManager
    , uvsClosed  :: {-# UNPACK #-} !(IORef Bool)
    }

-- | Safely lock an uv manager and perform uv_handle initialization.
--
-- Initialization an UV handle usually take two step:
--
--   * allocate an uv_handle struct with proper size
--   * lock a particular uv_loop from a uv manager, and perform custom initialization, such as @uv_tcp_init@.
--
-- And this is what 'initUVHandle' do, all you need to do is to provide the manager you want to hook the handle
-- onto(usually the one on the same capability, i.e. the one obtained by 'getUVManager'),
-- and provide a custom initialization function.
--
initUVStream :: HasCallStack
             => (Ptr UVLoop -> Ptr UVHandle -> IO ())
             -> UVManager
             -> Resource UVStream
initUVStream init uvm = initResource
    (withUVManager uvm $ \ loop -> do
        handle <- hs_uv_handle_alloc loop
        slot <- getUVSlot uvm (peekUVHandleData handle)
        tryTakeMVar =<< getBlockMVar uvm slot   -- clear the parking spot
        init loop handle `onException` hs_uv_handle_free handle
        closed <- newIORef False
        return (UVStream handle slot uvm closed))
    (\ (UVStream handle _ uvm closed) -> withUVManager_ uvm $ do
        c <- readIORef closed
        unless c $ do
            writeIORef closed True
            hs_uv_handle_close handle)

instance Input UVStream where
    -- readInput :: HasCallStack => UVStream -> Ptr Word8 ->  Int -> IO Int
    readInput (UVStream handle slot uvm closed) buf len = do
        c <- readIORef closed
        when c throwECLOSED
        m <- getBlockMVar uvm slot
        withUVManager_ uvm $ do
            throwUVIfMinus_ (hs_uv_read_start handle)
            pokeBufferTable uvm slot buf len
            tryTakeMVar m
        r <- takeMVar m
        if  | r > 0  -> return r
            -- r == 0 should be impossible, since we guard this situation in c side
            | r == fromIntegral uV_EOF -> return 0
            | r < 0 ->  throwUVIfMinus (return r)

instance Output UVStream where
    -- writeOutput :: HasCallStack => UVStream -> Ptr Word8 -> Int -> IO ()
    writeOutput (UVStream handle _ uvm closed) buf len = do
        c <- readIORef closed
        when c throwECLOSED
        m <- withUVManager_ uvm $ do
            slot <- getUVSlot uvm (hs_uv_write handle buf len)
            m <- getBlockMVar uvm slot
            tryTakeMVar m
            return m
        throwUVIfMinus_  (takeMVar m)

--------------------------------------------------------------------------------

withUVManagerWrap :: HasCallStack
                  => UVManager -> (Ptr UVLoop -> IO UVSlotUnSafe) -> IO Int
withUVManagerWrap uvm f = do
    m <- withUVManager uvm $ \ loop -> do
        slot <- getUVSlot uvm (f loop)
        m <- getBlockMVar uvm slot
        tryTakeMVar m
        return m
    throwUVIfMinus (takeMVar m)

withUVManagerWrap_ :: HasCallStack
                   => UVManager -> (Ptr UVLoop -> IO UVSlotUnSafe) -> IO ()
withUVManagerWrap_ uvm f = do
    m <- withUVManager uvm $ \ loop -> do
        slot <- getUVSlot uvm (f loop)
        m <- getBlockMVar uvm slot
        tryTakeMVar m
        return m
    throwUVIfMinus_ (takeMVar m)

--------------------------------------------------------------------------------

-- | Fork a new GHC thread with active load-balancing.
--
-- Using libuv based I/O solution has a disadvantage that file handlers are bound to certain
-- uv_loop, thus certain uv mananger/capability. Worker threads that migrate to other capability
-- may facing contention since various API here is protected by a running lock, this makes GHC's
-- work-stealing strategy unsuitable for certain workload, such as a webserver.
-- we solve this problem with simple round-robin load-balancing: forkBa will automatically
-- distribute your new threads to all capabilities in round-robin manner. Thus its name forkBa(lance).
--
forkBa :: IO () -> IO ThreadId
forkBa io = do
    i <- atomicAddCounter_ counter 1
    forkOn i io
  where
    counter :: Counter
    {-# NOINLINE counter #-}
    counter = unsafePerformIO $ newCounter 0
