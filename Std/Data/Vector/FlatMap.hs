{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Std.Data.Vector.FlatMap
  ( -- * FlatMap backed by sorted vector
    FlatMap, sortedKeyValues, size, null, empty, map', kmap'
  , pack, packN, packR, packRN
  , unpack, unpackR, packVector, packVectorR
  , lookup
  , delete
  , insert
  , adjust'
  , merge, mergeWithKey'
    -- * fold and traverse
  , foldrWithKey, foldrWithKey', foldlWithKey, foldlWithKey', traverseWithKey
    -- * binary & linear search on vectors
  , binarySearch
  , linearSearch, linearSearchR
  ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Primitive.SmallArray as A
import qualified Data.Foldable             as Foldable
import qualified Data.Traversable          as Traversable
import qualified Data.Semigroup            as Semigroup
import qualified Data.Monoid               as Monoid
import qualified Std.Data.Vector.Base as V
import qualified Std.Data.Vector.Sort as V
import qualified Std.Data.Text             as T
import qualified Std.Data.TextBuilder      as T
import           Data.Function              (on)
import           Data.Bits                   (shiftR)
import           Data.Data
import           Data.Typeable
import           Prelude hiding (lookup, null)
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))

--------------------------------------------------------------------------------

newtype FlatMap k v = FlatMap { sortedKeyValues :: V.Vector (k, v) }
    deriving (Show, Eq, Ord, Typeable)

instance (T.ToText k, T.ToText v) => T.ToText (FlatMap k v) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder p (FlatMap vec) = T.parenWhen (p > 10) $ do
        T.unsafeFromBuilder "FlatMap {"
        T.intercalateVec T.comma (\ (k, v) ->
            T.toTextBuilder 0 k >> ":" >> T.toTextBuilder 0 v) vec
        T.char7 '}'

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (FlatMap k v) where
    arbitrary = pack <$> arbitrary
    shrink v = pack <$> shrink (unpack v)

instance (CoArbitrary k, CoArbitrary v) => CoArbitrary (FlatMap k v) where
    coarbitrary = coarbitrary . unpack

instance Ord k => Semigroup.Semigroup (FlatMap k v) where
    {-# INLINE (<>) #-}
    (<>) = merge

instance Ord k => Monoid.Monoid (FlatMap k v) where
    {-# INLINE mappend #-}
    mappend = merge
    {-# INLINE mempty #-}
    mempty = empty

instance (NFData k, NFData v) => NFData (FlatMap k v) where
    {-# INLINE rnf #-}
    rnf (FlatMap kvs) = rnf kvs

instance Functor (FlatMap k) where
    {-# INLINE fmap #-}
    fmap f (FlatMap vs) = FlatMap (V.map' (fmap f) vs)

instance Foldable.Foldable (FlatMap k) where
    {-# INLINE foldr' #-}
    foldr' f = foldrWithKey' (const f)
    {-# INLINE foldr #-}
    foldr f = foldrWithKey (const f)
    {-# INLINE foldl' #-}
    foldl' f = foldlWithKey' (\ a k v -> f a v)
    {-# INLINE foldl #-}
    foldl f = foldlWithKey (\ a k v -> f a v)
    {-# INLINE toList #-}
    toList = fmap snd . unpack
    {-# INLINE null #-}
    null (FlatMap vs) = V.null vs
    {-# INLINE length #-}
    length (FlatMap vs) = V.length vs
    {-# INLINE elem #-}
    elem a (FlatMap vs) = elem a (map snd $ V.unpack vs)

instance Traversable.Traversable (FlatMap k) where
    {-# INLINE traverse #-}
    traverse f = traverseWithKey (const f)

size :: FlatMap k v -> Int
{-# INLINE size #-}
size = V.length . sortedKeyValues

null :: FlatMap k v -> Bool
{-# INLINE null #-}
null = V.null . sortedKeyValues

map' :: (v -> v') -> FlatMap k v -> FlatMap k v'
{-# INLINE map' #-}
map' f (FlatMap vs) = FlatMap (V.map' (fmap f) vs)

kmap' :: (k -> v -> v') -> FlatMap k v -> FlatMap k v'
{-# INLINE kmap' #-}
kmap' f (FlatMap vs) = FlatMap (V.map' (\ (k, v) -> (k, f k v)) vs)

-- | /O(1)/ empty flat map.
empty :: FlatMap k v
{-# INLINE empty #-}
empty = FlatMap V.empty

-- | /O(N*logN)/ Pack list of key values, on key duplication prefer left one.
pack :: Ord k => [(k, v)] -> FlatMap k v
{-# INLINE pack #-}
pack kvs = FlatMap (V.mergeDupAdjacentLeft ((==) `on` fst) (V.mergeSortBy (compare `on` fst) (V.pack kvs)))

-- | /O(N*logN)/ Pack list of key values with suggested size, on key duplication prefer left one.
packN :: Ord k => Int -> [(k, v)] -> FlatMap k v
{-# INLINE packN #-}
packN n kvs = FlatMap (V.mergeDupAdjacentLeft ((==) `on` fst) (V.mergeSortBy (compare `on` fst) (V.packN n kvs)))

-- | /O(N*logN)/ Pack list of key values, on key duplication prefer right one.
packR :: Ord k => [(k, v)] -> FlatMap k v
{-# INLINE packR #-}
packR kvs = FlatMap (V.mergeDupAdjacentRight ((==) `on` fst) (V.mergeSortBy (compare `on` fst) (V.pack kvs)))

-- | /O(N*logN)/ Pack list of key values with suggested size, on key duplication prefer right one.
packRN :: Ord k => Int -> [(k, v)] -> FlatMap k v
{-# INLINE packRN #-}
packRN n kvs = FlatMap (V.mergeDupAdjacentRight ((==) `on` fst) (V.mergeSortBy (compare `on` fst) (V.packN n kvs)))

-- | /O(N)/ Unpack key value pairs to a list sorted by keys in ascending order.
--
-- This function works with @foldr/build@ fusion in base.
unpack :: FlatMap k v -> [(k, v)]
{-# INLINE unpack #-}
unpack = V.unpack . sortedKeyValues

-- | /O(N)/ Unpack key value pairs to a list sorted by keys in descending order.
--
-- This function works with @foldr/build@ fusion in base.
unpackR :: FlatMap k v -> [(k, v)]
{-# INLINE unpackR #-}
unpackR = V.unpackR . sortedKeyValues

-- | /O(N*logN)/ Pack vector of key values, on key duplication prefer left one.
packVector :: Ord k => V.Vector (k, v) -> FlatMap k v
{-# INLINE packVector #-}
packVector kvs = FlatMap (V.mergeDupAdjacentLeft ((==) `on` fst) (V.mergeSortBy (compare `on` fst) kvs))

-- | /O(N*logN)/ Pack vector of key values, on key duplication prefer right one.
packVectorR :: Ord k => V.Vector (k, v) -> FlatMap k v
{-# INLINE packVectorR #-}
packVectorR kvs = FlatMap (V.mergeDupAdjacentRight ((==) `on` fst) (V.mergeSortBy (compare `on` fst) kvs))

-- | /O(logN)/ Binary search on flat map.
lookup :: Ord k => k -> FlatMap k v -> Maybe v
{-# INLINABLE lookup #-}
lookup _  (FlatMap (V.Vector arr s 0)) = Nothing
lookup k' (FlatMap (V.Vector arr s l)) = go s (s+l-1)
  where
    go !s !e
        | s == e =
            case arr `A.indexSmallArray` s of (k, v)  | k == k'  -> Just v
                                                        | otherwise -> Nothing
        | s >  e = Nothing
        | otherwise =
            let mid = (s+e) `shiftR` 1
                (k, v)  = arr `A.indexSmallArray` mid
            in case k' `compare` k of LT -> go s (mid-1)
                                      GT -> go (mid+1) e
                                      _  -> Just v

-- | /O(N)/ Insert new key value into map, replace old one if key exists.
insert :: Ord k => k -> v -> FlatMap k v -> FlatMap k v
{-# INLINE insert #-}
insert k v (FlatMap vec@(V.Vector arr s l)) =
    case binarySearch vec k of
        Left i -> FlatMap (V.create (l+1) (\ marr -> do
            when (i>s) $ A.copySmallArray marr 0 arr s (i-s)
            A.writeSmallArray marr i (k, v)
            when (i<(s+l)) $ A.copySmallArray marr (i+1) arr i (s+l-i)))
        Right i -> FlatMap (V.Vector (runST (do
            let arr' = A.cloneSmallArray arr s l
            marr <- A.unsafeThawSmallArray arr'
            A.writeSmallArray marr i (k, v)
            A.unsafeFreezeSmallArray marr)) 0 l)

-- | /O(N)/ Delete a key value pair by key.
delete :: Ord k => k -> FlatMap k v -> FlatMap k v
{-# INLINE delete #-}
delete k m@(FlatMap vec@(V.Vector arr s l)) =
    case binarySearch vec k of
        Left i -> m
        Right i -> FlatMap $ V.create (l-1) (\ marr -> do
            when (i>s) $ A.copySmallArray marr 0 arr s (i-s)
            let !end = s+l
                !j = i+1
            when (end > j) $ A.copySmallArray marr 0 arr j (end-j))

-- | /O(N)/ Modify a value by key.
--
-- The value is evaluated to WHNF before writing into map.
adjust' :: Ord k => (v -> v) -> k -> FlatMap k v -> FlatMap k v
{-# INLINE adjust' #-}
adjust' f k m@(FlatMap vec@(V.Vector arr s l)) =
    case binarySearch vec k of
        Left i -> m
        Right i -> FlatMap $ V.create l (\ marr -> do
            A.copySmallArray marr 0 arr s l
            let !v' = f (snd (A.indexSmallArray arr i))
            A.writeSmallArray marr i (k, v'))

-- | /O(n+m)/ Merge two 'FlatMap', prefer right value on key duplication.
merge :: forall k v. Ord k => FlatMap k v -> FlatMap k v -> FlatMap k v
{-# INLINE merge #-}
merge fmL@(FlatMap (V.Vector arrL sL lL)) fmR@(FlatMap (V.Vector arrR sR lR))
    | null fmL = fmR
    | null fmR = fmL
    | otherwise = FlatMap (V.createN (lL+lR) (go sL sR 0))
  where
    endL = sL + lL
    endR = sR + lR
    go :: Int -> Int -> Int -> A.SmallMutableArray s (k, v) -> ST s Int
    go !i !j !k marr
        | i >= endL = do
            A.copySmallArray marr k arrR j (lR-j)
            return $! k+lR-j
        | j >= endR = do
            A.copySmallArray marr k arrL i (lL-i)
            return $! k+lL-i
        | otherwise = do
            kvL@(kL, vL) <- arrL `A.indexSmallArrayM` i
            kvR@(kR, vR) <- arrR `A.indexSmallArrayM` j
            case kL `compare` kR of LT -> do A.writeSmallArray marr k kvL
                                             go (i+1) j (k+1) marr
                                    EQ -> do A.writeSmallArray marr k kvR
                                             go (i+1) (j+1) (k+1) marr
                                    _  -> do A.writeSmallArray marr k kvR
                                             go i (j+1) (k+1) marr

-- | /O(n+m)/ Merge two 'FlatMap' with a merge function.
mergeWithKey' :: forall k v. Ord k => (k -> v -> v -> v) -> FlatMap k v -> FlatMap k v -> FlatMap k v
{-# INLINABLE mergeWithKey' #-}
mergeWithKey' f fmL@(FlatMap (V.Vector arrL sL lL)) fmR@(FlatMap (V.Vector arrR sR lR))
    | null fmL = fmR
    | null fmR = fmL
    | otherwise = FlatMap (V.createN (lL+lR) (go sL sR 0))
  where
    endL = sL + lL
    endR = sR + lR
    go :: Int -> Int -> Int -> A.SmallMutableArray s (k, v) -> ST s Int
    go !i !j !k marr
        | i >= endL = do
            A.copySmallArray marr k arrR j (lR-j)
            return $! k+lR-j
        | j >= endR = do
            A.copySmallArray marr k arrL i (lL-i)
            return $! k+lL-i
        | otherwise = do
            kvL@(kL, vL) <- arrL `A.indexSmallArrayM` i
            kvR@(kR, vR) <- arrR `A.indexSmallArrayM` j
            case kL `compare` kR of LT -> do A.writeSmallArray marr k kvL
                                             go (i+1) j (k+1) marr
                                    EQ -> do let !v' = f kL vL vR
                                             A.writeSmallArray marr k (kL, v')
                                             go (i+1) (j+1) (k+1) marr
                                    _  -> do A.writeSmallArray marr k kvR
                                             go i (j+1) (k+1) marr


-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
--
-- During folding k is in descending order.
foldrWithKey :: (k -> v -> a -> a) -> a -> FlatMap k v -> a
{-# INLINE foldrWithKey #-}
foldrWithKey f a (FlatMap vs) = foldr (uncurry f) a vs

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
--
-- During folding k is in ascending order.
foldlWithKey :: (a -> k -> v -> a) -> a -> FlatMap k v -> a
{-# INLINE foldlWithKey #-}
foldlWithKey f a (FlatMap vs) = foldl (\ a' (k,v) -> f a' k v) a vs

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
--
-- During folding k is in descending order.
foldrWithKey' :: (k -> v -> a -> a) -> a -> FlatMap k v -> a
{-# INLINE foldrWithKey' #-}
foldrWithKey' f a (FlatMap vs) = V.foldr' (uncurry f) a vs

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
--
-- During folding k is in ascending order.
foldlWithKey' :: (a -> k -> v -> a) -> a -> FlatMap k v -> a
{-# INLINE foldlWithKey' #-}
foldlWithKey' f a (FlatMap vs) = V.foldl' (\ a' (k,v) -> f a' k v) a vs

-- | /O(n)/.
-- @'traverseWithKey' f s == 'pack' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('unpack' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
traverseWithKey :: Applicative t => (k -> a -> t b) -> FlatMap k a -> t (FlatMap k b)
{-# INLINE traverseWithKey #-}
traverseWithKey f (FlatMap vs) = FlatMap <$> traverse (\ (k,v) -> (k,) <$> f k v) vs

--------------------------------------------------------------------------------

-- | Find the key's index in the vector slice, if key exists return 'Right',
-- otherwise 'Left', i.e. the insert index
--
-- This function only works on ascending sorted vectors.
binarySearch :: Ord k => V.Vector (k, v) -> k -> Either Int Int
{-# INLINABLE binarySearch #-}
binarySearch (V.Vector arr s 0) _   = Left 0
binarySearch (V.Vector arr s l) !k' = go s (s+l-1)
  where
    go !s !e
        | s == e =
            let (k, v)  = arr `A.indexSmallArray` s
            in case k' `compare` k of LT -> Left s
                                      GT -> let !s' = s+1 in Left s'
                                      _  -> Right s
        | s >  e = Left s
        | otherwise =
            let !mid = (s+e) `shiftR` 1
                (k, v)  = arr `A.indexSmallArray` mid
            in case k' `compare` k of LT -> go s (mid-1)
                                      GT -> go (mid+1) e
                                      _  -> Right mid

--------------------------------------------------------------------------------

-- | linear scan search from left to right, return the first one if exist.
linearSearch :: Ord k => V.Vector (k, v) -> k -> Maybe v
{-# INLINABLE linearSearch #-}
linearSearch (V.Vector arr s l) !k' = go s
  where
    !end = s + l
    go !i
        | i >= end = Nothing
        | otherwise =
            let (k, v)  = arr `A.indexSmallArray` i
            in if k' == k then Just v else go (i+1)

-- | linear scan search from right to left, return the first one if exist.
linearSearchR :: Ord k => V.Vector (k, v) -> k -> Maybe v
{-# INLINABLE linearSearchR #-}
linearSearchR (V.Vector arr s l) !k' = go (s+l-1)
  where
    go !i
        | i < s = Nothing
        | otherwise =
            let (k, v)  = arr `A.indexSmallArray` i
            in if k' == k then Just v else go (i-1)
