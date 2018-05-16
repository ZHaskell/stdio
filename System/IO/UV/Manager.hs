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
  , withUVManager'
  -- * handle/request resources
  , initUVHandle
  , initUVHandleNoSlot
  , initUVReq
  , initUVReqNoSlot
  , initUVFS
  , initUVFSNoSlot
  , UVStream(..)
  , initUVStream
  , initTCPStream
  , initTCPExStream
  , initTTYStream
  , initPipeStream
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

#define IDLE_LIMIT 20

--------------------------------------------------------------------------------

data UVManager = UVManager
    { uvmBlockTable   :: {-# UNPACK #-} !(IORef (UnliftedArray (MVar ()))) -- a array to store threads blocked on async I/O.

    , uvmLoop        :: {-# UNPACK #-} !(Ptr UVLoop)        -- the uv loop refrerence

    , uvmLoopData    :: {-# UNPACK #-} !(Ptr UVLoopData)    -- cached pointer to uv_loop_t's data field

    , uvmRunning     :: {-# UNPACK #-} !(MVar Bool)     -- only uv manager thread will modify this value.
                                                        -- 'True' druing uv_run and 'False' otherwise.
                                                        --
                                                        -- unlike epoll/ONESHOT, uv loop are NOT thread safe,
                                                        -- we have to wake up the loop before mutating uv_loop's
                                                        -- state.

    , uvmAsync       :: {-# UNPACK #-} !(Ptr UVHandle)  -- This async handle is used when we want to break from
                                                        -- a blocking uv_run, send async request is the only
                                                        -- thread safe wake up mechanism for libuv.


    , uvmTimer       :: {-# UNPACK #-} !(Ptr UVHandle)  -- This timer handle is used to achieve polling with
                                                        -- given timeout, only used on non-threaded rts.

    , uvmCap ::  {-# UNPACK #-} !Int                -- the capability uv manager run on.
    }

instance Show UVManager where
    show uvm = "UVManager on capability " ++ show (uvmCap uvm)

instance Eq UVManager where
    uvm == uvm' =
        uvmCap uvm == uvmCap uvm'

initTableSize :: Int
initTableSize = 64

uvManagerArray :: IORef (Array UVManager)
{-# NOINLINE uvManagerArray #-}
uvManagerArray = unsafePerformIO $ do
    numCaps <- getNumCapabilities
    uvmArray <- newArr numCaps
    s <- newQSemN 0
    forM_ [0..numCaps-1] $ \ i -> do
        -- fork uv manager thread
        forkOn i . withResource (initUVManager initTableSize i) $ \ m -> do
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
getBlockMVar :: UVManager -> UVSlot -> IO (MVar ())
{-# INLINABLE getBlockMVar #-}
getBlockMVar uvm slot = do
    blockTable <- readIORef (uvmBlockTable uvm)
    indexArrM blockTable slot

-- | Poke a prepared buffer and size into loop data under given slot.
--
-- NOTE, this action is not protected with 'withUVManager' for effcient reason, you should merge this action
-- with other uv action and put them together inside a 'withUVManager' or 'withUVManager\''. for example:
--
--  @@@
--      ...
--      withUVManager' uvm $ do
--          pokeBufferTable uvm rslot buf len
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
    async <- initUVAsyncWake loop
    timer <- initUVTimer loop

    liftIO $ do
        mblockTable <- newArr siz
        forM_ [0..siz-1] $ \ i -> writeArr mblockTable i =<< newEmptyMVar
        blockTable <- unsafeFreezeArr mblockTable
        blockTableRef <- newIORef blockTable
        loopData <- peekUVLoopData loop
        running <- newMVar False
        return (UVManager blockTableRef loop loopData running async timer cap)

-- | Lock an uv mananger, so that we can safely mutate its uv_loop's state.
--
-- libuv is not thread safe, use this function to perform any action which will mutate uv_loop's state.
--
withUVManager :: HasCallStack => UVManager -> (Ptr UVLoop -> IO a) -> IO a
withUVManager uvm f = do

    r <- withMVar (uvmRunning uvm) $ \ running ->
        if running
        then do
            uvAsyncSend (uvmAsync uvm) -- if uv_run is running, it will stop
                                       -- if uv_run is not running, next running won't block
            return Nothing
        else do
            r <- f (uvmLoop uvm)
            return (Just r)

    case r of
        Just r' -> return r'
        _       -> yield >> withUVManager uvm f -- we yield here, because uv_run is probably not finished yet

-- | Lock an uv mananger, so that we can safely mutate its uv_loop's state.
--
-- Some action did not request uv_loop pointer explicitly, but will mutate uv_loop underhood, for example:
-- @uv_read_start@. These actions have to be protected by locking the uv_loop.
--
-- In fact most of the libuv's functions are not thread safe, so watch out!
--
withUVManager' :: HasCallStack => UVManager -> IO a -> IO a
withUVManager' uvm f = withUVManager uvm (\ _ -> f)

-- | Start the uv loop
--
startUVManager :: HasCallStack => UVManager -> IO ()
startUVManager uvm@(UVManager _ _ _ running _ _ _) = loop -- use a closure capture uvm in case of stack memory leaking
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
    step :: UVManager -> Bool -> IO CSize
    step (UVManager blockTableRef loop loopData _ _ timer _) block = do
            blockTable <- readIORef blockTableRef
            clearUVEventCounter loopData        -- clean event counter

            if block
            then if rtsSupportsBoundThreads
                then uvRunSafe loop uV_RUN_ONCE
                else do
                    -- use a 2ms timeout blocking poll on non-threaded rts
                    uvTimerWakeStart timer 2
                    uvRun loop uV_RUN_ONCE
            else uvRun loop uV_RUN_NOWAIT

            (c, q) <- peekUVEventQueue loopData
            forM_ [0..(fromIntegral c-1)] $ \ i -> do
                slot <- peekElemOff q i
                lock <- indexArrM blockTable (fromIntegral slot)
                tryPutMVar lock ()   -- unlock ghc thread with MVar
            return c

--------------------------------------------------------------------------------

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
initUVHandle :: HasCallStack
             => UVHandleType
             -> (Ptr UVLoop -> Ptr UVHandle -> IO ())
             -> UVManager
             -> Resource (Ptr UVHandle)
initUVHandle typ init uvm = initResource
    (withUVManager uvm $ \ loop -> do
        handle <- throwOOMIfNull $ hs_uv_handle_alloc typ loop
        slot <- peekUVHandleData handle
        autoResizeUVM uvm slot
        tryTakeMVar =<< getBlockMVar uvm slot   -- clear the parking spot
        init loop handle `onException` hs_uv_handle_free handle
        return handle)
    (withUVManager' uvm . hs_uv_handle_close) -- handle is free in uv_close callback

-- | Safely lock an uv manager and perform uv_handle initialization without allocating a slot.
--
-- This function allocate a handle without pre-allocated slot(data field), then perform initialization.
--
initUVHandleNoSlot :: HasCallStack
                   => UVHandleType
                   -> (Ptr UVLoop -> Ptr UVHandle -> IO ())
                   -> UVManager
                   -> Resource (Ptr UVHandle)
initUVHandleNoSlot typ init uvm = initResource
    (do handle <- throwOOMIfNull $ hs_uv_handle_alloc_no_slot typ
        withUVManager uvm $ \ loop ->
            init loop handle `onException` hs_uv_handle_free handle
        return handle)
    (withUVManager' uvm . hs_uv_handle_close_no_slot) -- handle is free in uv_close callback

-- | Safely lock an uv manager and perform uv_req initialization.
--
initUVReq :: HasCallStack
          => UVReqType
          -> (Ptr UVLoop -> Ptr UVReq -> IO ())
          -> UVManager
          -> Resource (Ptr UVReq)
initUVReq typ init uvm = initResource
    (withUVManager uvm $ \ loop -> do
        req <- throwOOMIfNull $ hs_uv_req_alloc typ loop
        slot <- peekUVReqData req
        autoResizeUVM uvm slot
        tryTakeMVar =<< getBlockMVar uvm slot   -- clear the parking spot
        init loop req `onException` hs_uv_req_free req loop
        return req)
    (\ req -> withUVManager uvm $ \ loop -> hs_uv_req_free req loop)

-- | Safely lock an uv manager without allocating a slot.
--
initUVReqNoSlot :: HasCallStack
                => UVReqType
                -> Resource (Ptr UVReq)
initUVReqNoSlot typ = initResource
    (throwOOMIfNull $ hs_uv_req_alloc_no_slot typ)
    hs_uv_req_free_no_slot

-- | Safely lock an uv manager and perform fs operation.
--
-- The `uv_fs_t` request has one more extra clean up step: `uv_fs_req_cleanup`, thus
-- for perform fs operation with `uv_fs_t` you should use this function instead of
-- `initUVReq`.
--
initUVFS :: HasCallStack
         => (Ptr UVLoop -> Ptr UVReq -> UVFSCallBack -> IO ())
         -> UVManager
         -> Resource (Ptr UVReq)
initUVFS fs uvm = initResource
    (withUVManager uvm $ \ loop -> do
        req <- throwOOMIfNull $ hs_uv_req_alloc uV_FS loop
        slot <- peekUVReqData req
        autoResizeUVM uvm slot
        tryTakeMVar =<< getBlockMVar uvm slot   -- clear the parking spot
        fs loop req uvFSCallBack `onException` hs_uv_req_free req loop
        return req)
    (\ req -> do
        uv_fs_req_cleanup req
        withUVManager uvm $ \ loop -> hs_uv_req_free req loop)

-- | Safely lock an uv manager and perform fs operation without allocating a slot.
--
-- This function will pass `nullPtr` as `Ptr UVLoop`, `nullFunPtr` as `UVFSCallBack`.
--
initUVFSNoSlot :: HasCallStack
               => (Ptr UVLoop -> Ptr UVReq -> UVFSCallBack -> IO ())
               -> Resource (Ptr UVReq)
initUVFSNoSlot fs = initResource
    (do req <- throwOOMIfNull $ hs_uv_req_alloc_no_slot uV_FS
        fs nullPtr req nullFunPtr `onException` hs_uv_req_free_no_slot req
        return req)
    (\ req -> uv_fs_req_cleanup req >> hs_uv_req_free_no_slot req)

autoResizeUVM :: UVManager -> Int -> IO ()
{-# INLINE autoResizeUVM #-}
autoResizeUVM (UVManager blockTableRef _ loopDataPtr _ _ _ _) slot = do
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

--------------------------------------------------------------------------------

-- | A higher level wrappe for uv_stream_t
--
--
data UVStream = UVStream
    { uvsHandle     :: {-# UNPACK #-} !(Ptr UVHandle)
    , uvsReadSlot   :: {-# UNPACK #-} !UVSlot
    , uvsWriteReq   :: {-# UNPACK #-} !(Ptr UVReq)
    , uvsWriteSlot  :: {-# UNPACK #-} !UVSlot
    , uvsManager    :: UVManager
    }

initUVStream :: HasCallStack
             => UVHandleType
             -> (Ptr UVLoop -> Ptr UVHandle -> IO ())
             -> UVManager
             -> Resource UVStream
initUVStream typ init uvm =
    initResource
        (do withUVManager uvm $ \ loop -> do
                handle <- throwOOMIfNull (hs_uv_handle_alloc typ loop)
                req <- throwOOMIfNull (hs_uv_req_alloc uV_WRITE loop)
                        `onException` (hs_uv_handle_free handle)
                init loop handle
                    `onException` (hs_uv_handle_free handle >> hs_uv_req_free req loop)

                rslot <- peekUVHandleData handle
                wslot <- peekUVReqData req
                autoResizeUVM uvm rslot
                autoResizeUVM uvm wslot
                return (UVStream handle rslot req wslot uvm))
        (\ (UVStream handle _ req  _ uvm) ->
            withUVManager uvm $ \ loop -> do
                hs_uv_handle_close handle -- handle is free in uv_close callback
                hs_uv_req_free req loop)

initTCPStream :: HasCallStack => UVManager -> Resource UVStream
initTCPStream = initUVStream uV_TCP uvTCPInit

initTCPExStream :: HasCallStack => CUInt -> UVManager -> Resource UVStream
initTCPExStream family = initUVStream uV_TCP (uvTCPInitEx family)

initPipeStream :: HasCallStack => UVManager -> Resource UVStream
initPipeStream = initUVStream uV_NAMED_PIPE uvPipeInit

initTTYStream :: HasCallStack => UVFD -> UVManager -> Resource UVStream
initTTYStream fd = initUVStream uV_TTY (uvTTYInit fd)

instance Input UVStream where
    -- readInput :: HasCallStack => UVStream -> Ptr Word8 ->  Int -> IO Int
    readInput uvs@(UVStream handle rslot _ _ uvm) buf len = do
        m <- getBlockMVar uvm rslot
        withUVManager' uvm $ do
            tryTakeMVar m
            pokeBufferTable uvm rslot buf len
            uvReadStart handle
        takeMVar m
        r <- peekBufferTable uvm rslot
        if  | r > 0  -> return r
            -- r == 0 should be impossible, since we guard this situation in c side, but we handle it anyway
            -- nread might be 0, which does not indicate an error or EOF. This is equivalent to EAGAIN or EWOULDBLOCK under read(2)
            | r == fromIntegral uV_EOF -> return 0
            | r < 0 ->  throwUVIfMinus (return r)

uvReadStart :: Ptr UVHandle -> IO ()
uvReadStart = throwUVIfMinus_ . hs_uv_read_start
foreign import ccall unsafe hs_uv_read_start :: Ptr UVHandle -> IO CInt

instance Output UVStream where
    -- writeOutput :: HasCallStack => UVStream -> Ptr Word8 -> Int -> IO ()
    writeOutput (UVStream handle _ req wslot uvm) buf len = do
        m <- getBlockMVar uvm wslot
        withUVManager' uvm $ do
            tryTakeMVar m
            pokeBufferTable uvm wslot buf len
            uvWrite req handle
        takeMVar m
        throwUVIfMinus_ $ peekBufferTable uvm wslot

uvWrite :: Ptr UVReq -> Ptr UVHandle -> IO ()
uvWrite req handle = throwUVIfMinus_ $ hs_uv_write req handle
foreign import ccall unsafe hs_uv_write :: Ptr UVReq -> Ptr UVHandle -> IO CInt

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
