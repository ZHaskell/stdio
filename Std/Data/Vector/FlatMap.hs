{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : Std.Data.Vector.FlatMap
Description : Fast map based on sorted vector
Copyright   : (c) Dong Han, 2017-2019
              (c) Tao He, 2018-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides a simple key value map based on sorted vector and binary search. It's particularly
suitable for small sized key value collections such as deserializing intermediate representation.
But can also used in various place where insertion and deletion is rare but require fast lookup.

-}

module Std.Data.Vector.FlatMap where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.Primitive.SmallArray as A
import qualified Std.Data.Vector.Base as V
import qualified Std.Data.Vector.Sort as V
import qualified Std.Data.Text as T
import           Data.Function              (on)
import           Data.Bits                   (shiftR)
import           Data.Data
import           Data.Typeable

-- | Type class for key value pair.
--
-- There're built-in instances for int key map, text key map and bytes key map, but it can be extended to support more
-- types. Let's say you may want to build a lookup table for 'User' type, writing an instance for this class. e.g.
--
-- @
--     data User = User { uid :: Int, uname :: T.Text, ... }
--     instance KeyVal
--         type Key User = Int
--         type Val User = User
--         toTuple u = (uid u, u)
--         keyVal k u = u{uid = k}  -- update is actually not neccessary
-- @
--
-- Now 'pack' a list of @User@s will build a @FlatMap User@, which can be 'lookup' via @uid@ key.
class Ord (Key kv) => KeyVal kv where
    type Key kv
    type Val kv
    toTuple :: kv -> (Key kv, Val kv)
    keyVal :: Key kv -> Val kv -> kv

-- | Bidirectional pattern synonyms for writing flat map literals, or matching a key value pair.
--
-- @pack ["foo" := ..., "bar" := ... ]  :: FlatMap (TextKV ...)@
pattern (:=) :: (KeyVal kv) => Key kv -> Val kv -> kv
pattern k := v <- (toTuple -> (k, v)) where
        k := v = keyVal k v

-- | Get key from a key value pair.
key :: KeyVal kv => kv -> Key kv
{-# INLINE key #-}
key = fst . toTuple

-- | Get value from a key value pair.
val :: KeyVal kv => kv -> Val kv
{-# INLINE val #-}
val = snd . toTuple

-- | @compare `on` key@
compareKey :: KeyVal kv => kv -> kv -> Ordering
{-# INLINE compareKey #-}
compareKey = compare `on` key

-- | @(==) `on` key@
eqKey :: KeyVal kv => kv -> kv -> Bool
{-# INLINE eqKey #-}
eqKey = (==) `on` key

-- | 'V.Bytes' as key.
data BytesKV v = BytesKV {-# UNPACK #-} !V.Bytes v
    deriving (Eq, Ord, Typeable, Show, Read)
-- | 'T.Text' as key.
data TextKV v = TextKV {-# UNPACK #-} !T.Text v
    deriving (Eq, Ord, Typeable, Show, Read)
-- | 'Int' as key.
data IntKV v = IntKV {-# UNPACK #-} !Int v
    deriving (Eq, Ord, Typeable, Show, Read)

instance KeyVal (BytesKV v) where
    type Key (BytesKV v) = V.Bytes
    type Val (BytesKV v) = v
    toTuple (BytesKV k v) = (k, v)
    {-# INLINE toTuple #-}
    keyVal = BytesKV
    {-# INLINE keyVal #-}

instance KeyVal (TextKV v) where
    type Key (TextKV v) = T.Text
    type Val (TextKV v) = v
    toTuple (TextKV k v) = (k, v)
    {-# INLINE toTuple #-}
    keyVal = TextKV
    {-# INLINE keyVal #-}

instance KeyVal (IntKV v) where
    type Key (IntKV v) = Int
    type Val (IntKV v) = v
    toTuple (IntKV k v) = (k, v)
    {-# INLINE toTuple #-}
    keyVal = IntKV
    {-# INLINE keyVal #-}

instance Ord k => KeyVal (k, v) where
    type Key (k, v) = k
    type Val (k, v) = v
    toTuple = id
    {-# INLINE toTuple #-}
    keyVal = (,)
    {-# INLINE keyVal #-}

--------------------------------------------------------------------------------

newtype FlatMap kv = FlatMap { sortedKV :: V.Vector kv } deriving (Show, Eq, Ord, Typeable)

-- | /O(1)/ empty flat map.
empty :: FlatMap a
{-# INLINE empty #-}
empty = FlatMap V.empty

-- | /O(N*logN)/ Pack list of key values, on key duplication prefer left one.
pack :: KeyVal kv => [kv] -> FlatMap kv
{-# INLINE pack #-}
pack kvs = FlatMap (V.mergeDupAdjacentLeft eqKey (V.mergeSortBy compareKey (V.pack kvs)))

-- | /O(N*logN)/ Pack list of key values with suggested size, on key duplication prefer left one.
packN :: KeyVal kv => Int -> [kv] -> FlatMap kv
{-# INLINE packN #-}
packN n kvs = FlatMap (V.mergeDupAdjacentLeft eqKey (V.mergeSortBy compareKey (V.packN n kvs)))

-- | /O(N*logN)/ Pack list of key values, on key duplication prefer right one.
packR :: KeyVal kv => [kv] -> FlatMap kv
{-# INLINE packR #-}
packR kvs = FlatMap (V.mergeDupAdjacentRight eqKey (V.mergeSortBy compareKey (V.pack kvs)))

-- | /O(N*logN)/ Pack list of key values with suggested size, on key duplication prefer right one.
packRN :: KeyVal kv => Int -> [kv] -> FlatMap kv
{-# INLINE packRN #-}
packRN n kvs = FlatMap (V.mergeDupAdjacentRight eqKey (V.mergeSortBy compareKey (V.packN n kvs)))

-- | /O(N)/ Unpack key value pairs to a list sorted by keys in ascending order.
--
-- This function works with @foldr/build@ fusion in base.
unpack :: FlatMap kv -> [kv]
{-# INLINE unpack #-}
unpack = V.unpack . sortedKV

-- | /O(N)/ Unpack key value pairs to a list sorted by keys in descending order.
--
-- This function works with @foldr/build@ fusion in base.
unpackR :: FlatMap kv -> [kv]
{-# INLINE unpackR #-}
unpackR = V.unpackR . sortedKV

-- | /O(logN)/ Binary search on flat map.
lookup :: KeyVal kv => Key kv -> FlatMap kv -> Maybe (Val kv)
{-# INLINABLE lookup #-}
lookup _  (FlatMap (V.Vector arr s 0)) = Nothing
lookup k' (FlatMap (V.Vector arr s l)) = go s (s+l-1)
  where
    go !s !e
        | s == e =
            case arr `A.indexSmallArray` s of (k := v)  | k == k'  -> Just v
                                                        | otherwise -> Nothing
        | s >  e = Nothing
        | otherwise =
            let mid = (s+e) `shiftR` 1
                (k := v)  = arr `A.indexSmallArray` mid
            in case k' `compare` k of LT -> go s (mid-1)
                                      GT -> go (mid+1) e
                                      _  -> Just v

-- | /O(N)/ Insert new key value into map, replace old one if key exists.
insert :: KeyVal kv => kv -> FlatMap kv -> FlatMap kv
{-# INLINABLE insert #-}
insert kv m@(FlatMap (V.Vector arr s l)) =
    case find m (key kv) of
        Left i -> FlatMap (V.create (l+1) (\ marr -> do
            when (i>s) $ A.copySmallArray marr 0 arr s (i-s)
            A.writeSmallArray marr i kv
            when (i<(s+l)) $ A.copySmallArray marr (i+1) arr i (s+l-i)))
        Right i -> FlatMap (V.Vector (runST (do
            let arr' = A.cloneSmallArray arr s l
            marr <- A.unsafeThawSmallArray arr'
            A.writeSmallArray marr i kv
            A.unsafeFreezeSmallArray marr)) 0 l)

-- | /O(N)/ Delete a key value pair by key.
delete :: KeyVal kv => Key kv -> FlatMap kv -> FlatMap kv
{-# INLINABLE delete #-}
delete k m@(FlatMap (V.Vector arr s l)) =
    case find m k of
        Left i -> m
        Right i -> FlatMap $ V.create (l-1) (\ marr -> do
            when (i>s) $ A.copySmallArray marr 0 arr s (i-s)
            let !end = s+l
                !j = i+1
            when (end > j) $ A.copySmallArray marr 0 arr j (end-j))

-- | /O(N)/ Modify a value by key.
--
-- The value is evaluated to WHNF before writing into map.
update' :: KeyVal kv => (Val kv -> Val kv) -> Key kv -> FlatMap kv -> FlatMap kv
{-# INLINABLE update' #-}
update' f k m@(FlatMap (V.Vector arr s l)) =
    case find m k of
        Left i -> m
        Right i -> FlatMap $ V.create l (\ marr -> do
            A.copySmallArray marr 0 arr s l
            let !v' = f (val (A.indexSmallArray arr i))
            A.writeSmallArray marr i (k := v'))

--------------------------------------------------------------------------------

-- | Find the key's index in the vector slice, if key exists return 'Right', otherwise 'Left'.
find :: KeyVal kv => FlatMap kv -> Key kv -> Either Int Int
{-# INLINABLE find #-}
find (FlatMap (V.Vector arr s 0)) _   = Left 0
find (FlatMap (V.Vector arr s l)) !k' = go s (s+l-1)
  where
    go !s !e
        | s == e =
            let (k := v)  = arr `A.indexSmallArray` s
            in case k' `compare` k of LT -> Left s
                                      GT -> let !s' = s+1 in Left s'
                                      _  -> Right s
        | s >  e = Left s
        | otherwise =
            let !mid = (s+e) `shiftR` 1
                (k := v)  = arr `A.indexSmallArray` mid
            in case k' `compare` k of LT -> go s (mid-1)
                                      GT -> go (mid+1) e
                                      _  -> Right mid
