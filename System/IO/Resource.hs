{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : System.IO.UV.Exception
Description : Extensible IO exceptions
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module also implements Gabriel Gonzalez'd idea on 'Resource' applicative:
<http://www.haskellforall.com/2013/06/the-resource-applicative.html>. The 'Applicative' and 'Monad' instance is
especially useful when you want safely combine multiple resources.
-}

module System.IO.Resource (
    -- * Resource management
    Resource(..)
  , initResource
  , initResource_
  , withResource
  , withResource'
    -- * Resource pool
  , Pool
  , PoolState(..)
  , initPool
  , statPool
  , usePool
) where

import Control.Monad.Catch
import Control.Monad
import Control.Monad.IO.Class
import System.LowResTimer
import qualified System.IO.Exception as E
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.IORef.Unboxed

--------------------------------------------------------------------------------

-- | A 'Resource' is an 'IO' action which acquires some resource of type a and
-- also returns a finalizer of type IO () that releases the resource.
--
-- The only safe way to use a 'Resource' is 'withResource' \/ 'withResource\'',
-- You should not use the `acquire` field directly, unless you want to implement your own
-- resource management. In the later case, you should always use 'E.mask_' since
-- some resource initializations may assume async exceptions are masked.
--
-- 'MonadIO' instance is provided so that you can lift 'IO' computation inside
-- 'Resource', this is convenient for propagating 'Resource' around since many
-- 'IO' computations carry finalizers.
--
-- A convention in stdio is that functions returning a 'Resource' should be
-- named in @initXXX@ format, users are strongly recommended to follow this convention.
--
newtype Resource a = Resource { acquire :: E.HasCallStack => IO (a, IO ()) }

-- | Create 'Resource' from create and release action.
--
-- Note, 'resource' doesn't open resource itself, resource is created when you use
-- 'with' \/ 'with''.
--
initResource :: IO a -> (a -> IO ()) -> Resource a
{-# INLINE initResource #-}
initResource create release = Resource $ do
    r <- create
    return $ (r, release r)

-- | Create 'Resource' from create and release action.
--
-- This function is useful when you want to add some initialization and clean up action
-- inside 'Resource' monad.
--
initResource_ :: IO () -> IO () -> Resource ()
{-# INLINE initResource_ #-}
initResource_ create release = Resource $ do
    r <- create
    return $ (r, release)

instance Functor Resource where
    {-# INLINE fmap #-}
    fmap f resource = Resource $ do
        (a, release) <- acquire resource
        return (f a, release)

instance Applicative Resource where
    {-# INLINE pure #-}
    pure a = Resource (pure (a, pure ()))
    {-# INLINE (<*>) #-}
    resource1 <*> resource2 = Resource $ do
        (f, release1) <- acquire resource1
        (x, release2) <- acquire resource2 `onException` release1
        return (f x, release2 >> release1)

instance Monad Resource where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    m >>= f = Resource $ do
        (m', release1) <- acquire m
        (x , release2) <- acquire (f m') `onException` release1
        return (x, release2 >> release1)

instance MonadIO Resource where
    {-# INLINE liftIO #-}
    liftIO f = Resource $ fmap (\ a -> (a, dummyRelease)) f
        where dummyRelease = return ()

-- | Create a new resource and run some computation, resource is guarantee to
-- be closed.
--
-- Be care don't leak the resource through computation return value, because
-- after the computation finishes, the resource is closed already.
--
withResource :: (MonadMask m, MonadIO m, E.HasCallStack)
             => Resource a -> (a -> m b) -> m b
{-# INLINABLE withResource #-}
withResource resource k = bracket (liftIO (acquire resource))
                                 (\(_, release) -> liftIO release)
                                 (\(a, _) -> k a)

-- | Create a new resource and run some computation, resource is guarantee to
-- be closed.
--
-- The difference from 'with' is that the computation will receive an extra
-- close action, which can be used to close the resource early before the whole
-- computation finished, the close action can be called multiple times,
-- only the first call will clean up the resource.
--
withResource' :: (MonadMask m, MonadIO m, E.HasCallStack)
              => Resource a -> (a -> m () -> m b) -> m b
{-# INLINABLE withResource' #-}
withResource' resource k = do
    c <- liftIO (newCounter 0)
    bracket (liftIO $ do
                (a, release) <- (acquire resource)
                let release' = do
                        c' <- atomicOrCounter_ c 1
                        when (c' == 0) release
                return (a, release'))
             (\(_, release) -> liftIO release)
             (\(a, release) -> k a (liftIO release))

--------------------------------------------------------------------------------

-- | A single resource pool entry.
data Entry a = Entry (a, IO ()) -- ^ the resource and clean up action
                     {-# UNPACK #-} !Int        -- ^ the life remaining

data PoolState = PoolClosed | PoolScanning | PoolEmpty deriving (Eq, Show)

-- | A high performance resource pool based on STM.
--
-- We choose to not divide pool into strips due to the difficults in resource balancing. If there
-- is a high contention on resource (see 'statPool'), just increase the maximum number of resources
-- can be opened.
--
data Pool a = Pool
    { poolResource :: Resource a
    , poolLimit :: Int
    , poolIdleTime :: Int
    , poolEntries :: TVar [Entry a]
    , poolInUse :: TVar Int
    , poolState :: TVar PoolState
    }

-- | Initialize a resource pool with given 'Resource'
--
-- Like other initXXX functions, this function won't open a resource pool until you use 'withResource'.
-- And this resource pool follow the same resource management pattern like other resources.
--
initPool :: Resource a
         -> Int     -- ^ maximum number of resources can be opened
         -> Int     -- ^ amount of time after which an unused resource can be released (in seconds).
         -> Resource (Pool a)
initPool res limit itime = initResource createPool closePool
  where
    createPool = do
        entries <- newTVarIO []
        inuse <- newTVarIO 0
        state <- newTVarIO PoolEmpty
        return (Pool res limit itime entries inuse state)

    closePool (Pool _ _ _ entries _ state) = do
        atomically $ writeTVar state PoolClosed
        es <- readTVarIO entries
        forM_ es $ \ (Entry (_, close) _) ->
            handleAll (\ _ -> return ()) close

-- | Get a resource pool's 'PoolState'
--
-- This function is useful when debug, under load lots of 'PoolEmpty' may indicate
-- contention on resources, i.e. the limit on maximum number of resources can be opened
-- should be adjusted to a higher number. On the otherhand, lots of 'PoolScanning'
-- may indicate there're too much free resources.
--
statPool :: Pool a -> IO PoolState
statPool pool = readTVarIO (poolState pool)

-- | Obtain the pooled resource inside a given resource pool.
--
-- You shouldn't use 'withResource' with this resource after you closed the pool,
-- an 'E.ResourceVanished' with @EPOOLCLOSED@ name will be thrown.
--
usePool :: Pool a -> Resource a
usePool (Pool res limit itime entries inuse state) = fst <$> initResource takeFromPool returnToPool
  where
    takeFromPool = join . atomically $ do
        c <- readTVar state
        if c == PoolClosed
        then throwSTM $ E.ResourceVanished
                (E.IOEInfo "EPOOLCLOSED" "resource pool is closed" E.callStack)
        else do
            es <- readTVar entries
            case es of
                ((Entry a _):es') -> do
                    writeTVar entries es'
                    return (return a)
                _ -> do
                    i <- readTVar inuse
                    when (i == limit) retry
                    modifyTVar' inuse (+1)
                    return (acquire res `onException`
                         atomically (modifyTVar' inuse (subtract 1)))

    returnToPool a = join . atomically $ do
        c <- readTVar state
        case c of
            PoolClosed -> return (snd a)
            PoolEmpty -> do
                modifyTVar' entries (Entry a itime:)
                writeTVar state PoolScanning
                return (void $ registerLowResTimer 10 (scanPool entries inuse state))
            _ -> do
                modifyTVar' entries (Entry a itime:)
                return (return ())

    scanPool entries inuse state = do
         join . atomically $ do
            c <- readTVar state
            if c == PoolClosed
            then return (return ())
            else do
                es <- readTVar entries
                if (null es)
                then do
                    writeTVar state PoolEmpty
                    return (return ())
                else do
                    let (deadNum, dead, living) = age es 0 [] []
                    writeTVar entries living
                    modifyTVar' inuse (subtract deadNum)
                    return (do
                        forM_ dead $ \ (_, close) -> handleAll (\ _ -> return ()) close
                        void $ registerLowResTimer 10 (scanPool entries inuse state))

    age ((Entry a life):es) !deadNum dead living
        | life > 1  = age es deadNum     dead     (Entry a (life-1):living)
        | otherwise = age es (deadNum+1) (a:dead) living
    age _ !deadNum dead living = (deadNum, dead, living)
