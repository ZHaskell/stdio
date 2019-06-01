{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Std.Data.Vector.FlatSet
Description : Fast map based on sorted vector
Copyright   : (c) Dong Han, 2017-2019
              (c) Tao He, 2018-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides a simple value set based on sorted vector and binary search. It's particularly
suitable for small sized value collections such as deserializing intermediate representation.
But can also used in various place where insertion and deletion is rare but require fast elem.

-}

module Std.Data.Vector.FlatSet
  ( -- * FlatSet backed by sorted vector
    FlatSet, sortedValues, size, null, empty, map'
  , pack, packN, packR, packRN
  , unpack, unpackR, packVector, packVectorR
  , elem
  , delete
  , insert
  , merge
    -- * binary & linear search on vectors
  , binarySearch
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
import qualified Std.Data.Vector.Search as V
import qualified Std.Data.Text as T
import qualified Std.Data.TextBuilder      as T
import           Data.Function              (on)
import           Data.Bits                   (shiftR)
import           Data.Data
import           Data.Typeable
import           Prelude hiding (elem, null)
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))

--------------------------------------------------------------------------------

newtype FlatSet v = FlatSet { sortedValues :: V.Vector v }
    deriving (Show, Eq, Ord, Typeable, Foldable, NFData)

instance T.ToText v => T.ToText (FlatSet v) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder p (FlatSet vec) = T.parenWhen (p > 10) $ do
        T.unsafeFromBuilder "FlatSet {"
        T.intercalateVec T.comma (T.toTextBuilder 0) vec
        T.char7 '}'

instance Ord v => Semigroup.Semigroup (FlatSet v) where
    {-# INLINE (<>) #-}
    (<>) = merge

instance Ord v => Monoid.Monoid (FlatSet v) where
    {-# INLINE mappend #-}
    mappend = merge
    {-# INLINE mempty #-}
    mempty = empty

instance (Ord v, Arbitrary v) => Arbitrary (FlatSet v) where
    arbitrary = pack <$> arbitrary
    shrink v = pack <$> shrink (unpack v)

instance (CoArbitrary v) => CoArbitrary (FlatSet v) where
    coarbitrary = coarbitrary . unpack

size :: FlatSet v -> Int
{-# INLINE size #-}
size = V.length . sortedValues

null :: FlatSet v -> Bool
{-# INLINE null #-}
null = V.null . sortedValues

-- | Mapping values of within a set, the result size may change if there're duplicated values
-- after mapping.
map' :: forall v. Ord v => (v -> v) -> FlatSet v -> FlatSet v
{-# INLINE map' #-}
map' f (FlatSet vs) = packVector (V.map' f vs :: V.Vector v)

-- | /O(1)/ empty flat map.
empty :: FlatSet v
{-# INLINE empty #-}
empty = FlatSet V.empty

-- | /O(N*logN)/ Pack list of key values, on key duplication prefer left one.
pack :: Ord v => [v] -> FlatSet v
{-# INLINE pack #-}
pack vs = FlatSet (V.mergeDupAdjacentLeft (==) (V.mergeSort (V.pack vs)))

-- | /O(N*logN)/ Pack list of key values with suggested size, on key duplication prefer left one.
packN :: Ord v => Int -> [v] -> FlatSet v
{-# INLINE packN #-}
packN n vs = FlatSet (V.mergeDupAdjacentLeft (==) (V.mergeSort (V.packN n vs)))

-- | /O(N*logN)/ Pack list of key values, on key duplication prefer right one.
packR :: Ord v => [v] -> FlatSet v
{-# INLINE packR #-}
packR vs = FlatSet (V.mergeDupAdjacentRight (==) (V.mergeSort (V.pack vs)))

-- | /O(N*logN)/ Pack list of key values with suggested size, on key duplication prefer right one.
packRN :: Ord v => Int -> [v] -> FlatSet v
{-# INLINE packRN #-}
packRN n vs = FlatSet (V.mergeDupAdjacentRight (==) (V.mergeSort (V.packN n vs)))

-- | /O(N)/ Unpack a set of values to a list s in ascending order.
--
-- This function works with @foldr/build@ fusion in base.
unpack :: FlatSet v -> [v]
{-# INLINE unpack #-}
unpack = V.unpack . sortedValues

-- | /O(N)/ Unpack a set of values to a list s in descending order.
--
-- This function works with @foldr/build@ fusion in base.
unpackR :: FlatSet v -> [v]
{-# INLINE unpackR #-}
unpackR = V.unpackR . sortedValues

-- | /O(N*logN)/ Pack vector of key values, on key duplication prefer left one.
packVector :: Ord v => V.Vector v -> FlatSet v
{-# INLINE packVector #-}
packVector vs = FlatSet (V.mergeDupAdjacentLeft (==) (V.mergeSort vs))

-- | /O(N*logN)/ Pack vector of key values, on key duplication prefer right one.
packVectorR :: Ord v => V.Vector v -> FlatSet v
{-# INLINE packVectorR #-}
packVectorR vs = FlatSet (V.mergeDupAdjacentRight (==) (V.mergeSort vs))

-- | /O(logN)/ Binary search on flat map.
elem :: Ord v => v -> FlatSet v -> Bool
{-# INLINABLE elem #-}
elem _ (FlatSet (V.Vector arr s 0)) = False
elem v (FlatSet vec) = case binarySearch vec v of Left _ -> False
                                                  _      -> True
-- | /O(N)/ Insert new key value into map, replace old one if key exists.
insert :: Ord v => v -> FlatSet v -> FlatSet v
{-# INLINE insert #-}
insert v m@(FlatSet vec@(V.Vector arr s l)) =
    case binarySearch vec v of
        Left i -> FlatSet (V.create (l+1) (\ marr -> do
            when (i>s) $ A.copySmallArray marr 0 arr s (i-s)
            A.writeSmallArray marr i v
            when (i<(s+l)) $ A.copySmallArray marr (i+1) arr i (s+l-i)))
        Right i -> m

-- | /O(N)/ Delete a key value pair by key.
delete :: Ord v => v -> FlatSet v -> FlatSet v
{-# INLINE delete #-}
delete v m@(FlatSet vec@(V.Vector arr s l)) =
    case binarySearch vec v of
        Left i -> m
        Right i -> FlatSet $ V.create (l-1) (\ marr -> do
            when (i>s) $ A.copySmallArray marr 0 arr s (i-s)
            let !end = s+l
                !j = i+1
            when (end > j) $ A.copySmallArray marr 0 arr j (end-j))

-- | /O(n+m)/ Merge two 'FlatSet', prefer right value on value duplication.
merge :: forall v . Ord v => FlatSet v -> FlatSet v -> FlatSet v
{-# INLINE merge #-}
merge fmL@(FlatSet (V.Vector arrL sL lL)) fmR@(FlatSet (V.Vector arrR sR lR))
    | null fmL = fmR
    | null fmR = fmL
    | otherwise = FlatSet (V.createN (lL+lR) (go sL sR 0))
  where
    endL = sL + lL
    endR = sR + lR
    go :: Int -> Int -> Int -> A.SmallMutableArray s v -> ST s Int
    go !i !j !k marr
        | i >= endL = do
            A.copySmallArray marr k arrR j (lR-j)
            return $! k+lR-j
        | j >= endR = do
            A.copySmallArray marr k arrL i (lL-i)
            return $! k+lL-i
        | otherwise = do
            vL <- arrL `A.indexSmallArrayM` i
            vR <- arrR `A.indexSmallArrayM` j
            case vL `compare` vR of LT -> do A.writeSmallArray marr k vL
                                             go (i+1) j (k+1) marr
                                    EQ -> do A.writeSmallArray marr k vR
                                             go (i+1) (j+1) (k+1) marr
                                    _  -> do A.writeSmallArray marr k vR
                                             go i (j+1) (k+1) marr

--------------------------------------------------------------------------------

-- | Find the key's index in the vector slice, if key exists return 'Right',
-- otherwise 'Left', i.e. the insert index
--
-- This function only works on ascending sorted vectors.
binarySearch :: Ord v => V.Vector v -> v -> Either Int Int
{-# INLINABLE binarySearch #-}
binarySearch (V.Vector arr s 0) _   = Left 0
binarySearch (V.Vector arr s l) !v' = go s (s+l-1)
  where
    go !s !e
        | s == e =
            let v = arr `A.indexSmallArray` s
            in case v' `compare` v of LT -> Left s
                                      GT -> let !s' = s+1 in Left s'
                                      _  -> Right s
        | s >  e = Left s
        | otherwise =
            let !mid = (s+e) `shiftR` 1
                v = arr `A.indexSmallArray` mid
            in case v' `compare` v of LT -> go s (mid-1)
                                      GT -> go (mid+1) e
                                      _  -> Right mid
