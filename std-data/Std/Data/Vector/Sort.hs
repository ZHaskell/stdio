{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

{-|
Module      : Std.Data.Vector.Sort
Description : Sorting vectors
Copyright   : (c) 2008-2011 Dan Doel, (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide three stable sorting algorithms, which are:

  * 'mergeSort', a /O(log(n))/ general-purpose sorting algorithms for all different size vectors.

  * 'insertSort' a /O(n^2)/ sorting algorithms suitable for very small vectors.

  * 'radixSort' a /O(n)/ sorting algorithms based on 'Radix' instance, which is prefered on large vectors.

Sorting is always performed in ascending order. To reverse the order, either use @XXSortBy@ or use 'Down', 'RadixDown' newtypes. In general changing comparing functions can be done by creating auxiliary newtypes and 'Ord' instances (make sure you inline instance's method for performence!). Or 'Radix' instances in 'radixSort' case, for example:

@
data Foo = Foo { key :: Int16, ... }

instance Radix Foo where
    -- You should add INLINE pragmas to following methods
    bucketSize = bucketSize . key
    passes = passes . key
    radixLSB = radixLSB . key
    radix i = radix i . key
    radixMSB = radixMSB . key
@

-}

module Std.Data.Vector.Sort (
  -- * Sort
    mergeSort
  , mergeSortBy
  , mergeTileSize
  , insertSort
  , insertSortBy
  , Down(..)
  , radixSort
  , Radix(..)
  , RadixDown(..)
  -- * merge duplicated
  , mergeDupAdjacent
  , mergeDupAdjacentLeft
  , mergeDupAdjacentRight
  , mergeDupAdjacentBy
  ) where

import           Control.Monad.ST
import           Data.Bits
import           Data.Int
import           Data.Ord               (Down (..))
import           Data.Primitive         (sizeOf)
import           Data.Primitive.Types   (Prim (..))
import           Data.Word
import           Prelude                hiding (splitAt)
import           Std.Data.Array
import           Std.Data.Vector.Base
import           Std.Data.Vector.Extra

--------------------------------------------------------------------------------
-- Comparison Sort

-- | /O(n*log(n))/ Sort vector based on element's 'Ord' instance with classic
-- <https://en.wikipedia.org/wiki/Merge_sort mergesort> algorithm.
--
-- This is a stable sort, During sorting two O(n) worker arrays are needed, one of
-- them will be freezed into the result vector. The merge sort only begin at tile
-- size larger than 'mergeTileSize', each tile will be sorted with 'insertSort', then
-- iteratively merged into larger array, until all elements are sorted.
mergeSort :: forall v a. (Vec v a, Ord a) => v a -> v a
{-# INLINABLE mergeSort #-}
mergeSort = mergeSortBy compare

mergeSortBy :: forall v a. Vec v a => (a -> a -> Ordering) -> v a -> v a
{-# INLINE mergeSortBy #-}
mergeSortBy cmp vec@(Vec _ _ l)
    | l <= mergeTileSize = insertSortBy cmp vec
    | otherwise = runST (do
        -- create two worker array
        w1 <- newArr l
        w2 <- newArr l
        firstPass vec 0 w1
        w <- mergePass w1 w2 mergeTileSize
        return $! fromArr w 0 l)
  where
    firstPass !v !i !marr
        | i >= l     = return ()
        | otherwise = do
            let (v',rest) = splitAt mergeTileSize v
            insertSortToMArr cmp v' i marr
            firstPass rest (i+mergeTileSize) marr

    mergePass !w1 !w2 !blockSiz
        | blockSiz >= l = unsafeFreezeArr w1
        | otherwise     = do
            mergeLoop w1 w2 blockSiz 0
            mergePass w2 w1 (blockSiz*2) -- swap worker array and continue merging

    mergeLoop !src !target !blockSiz !i
        | i >= l-blockSiz =                 -- remaining elements less than a block
            if i >= l
            then return ()
            else copyMutableArr target i src i (l-i)
        | otherwise = do
            let !mergeEnd = min (i+blockSiz+blockSiz) l
            mergeBlock src target (i+blockSiz) mergeEnd i (i+blockSiz) i
            mergeLoop src target blockSiz mergeEnd

    mergeBlock !src !target !leftEnd !rightEnd !i !j !k = do
        lv <- readArr src i
        rv <- readArr src j
        case rv `cmp` lv of
            LT -> do
                writeArr target k rv
                let !j' = j + 1
                    !k' = k + 1
                if j' >= rightEnd
                then copyMutableArr target k' src i (leftEnd - i)
                else mergeBlock src target leftEnd rightEnd i j' k'
            _ -> do
                writeArr target k lv
                let !i' = i + 1
                    !k' = k + 1
                if i' >= leftEnd
                then copyMutableArr target k' src j (rightEnd - j)
                else mergeBlock src target leftEnd rightEnd i' j k'

-- | The mergesort tile size, @mergeTileSize = 8@.
mergeTileSize :: Int
{-# INLINE mergeTileSize #-}
mergeTileSize = 8

-- | /O(n^2)/ Sort vector based on element's 'Ord' instance with simple
-- <https://en.wikipedia.org/wiki/Insertion_sort insertion-sort> algorithm.
--
-- This is a stable sort. O(n) extra space are needed,
-- which will be freezed into result vector.
insertSort :: (Vec v a, Ord a) => v a -> v a
{-# INLINE insertSort #-}
insertSort = insertSortBy compare

insertSortBy :: Vec v a => (a -> a -> Ordering) -> v a -> v a
{-# INLINE insertSortBy #-}
insertSortBy cmp v@(Vec _ _ l) | l <= 1 = v
                               | otherwise = create l (insertSortToMArr cmp v 0)

insertSortToMArr  :: Vec v a
                  => (a -> a -> Ordering)
                  -> v a            -- the original vector
                  -> Int            -- writing offset in the mutable array
                  -> MArray v s a   -- writing mutable array, must have enough space!
                  -> ST s ()
{-# INLINE insertSortToMArr #-}
insertSortToMArr cmp (Vec arr s l) moff marr = go s
  where
    !end = s + l
    !doff = moff-s
    go !i | i >= end  = return ()
          | otherwise = case indexArr' arr i of
               (# x #) -> do insert x (i+doff)
                             go (i+1)
    insert !temp !i
        | i <= moff = do
            writeArr marr moff temp
        | otherwise = do
            x <- readArr marr (i-1)
            case temp `cmp` x of
                LT -> do
                    writeArr marr i x
                    insert temp (i-1)
                _ -> writeArr marr i temp

--------------------------------------------------------------------------------
-- Radix Sort

-- | Types contain radixs, which can be inspected with 'radix' during different 'passes'.
--
-- The default instances share a same 'bucketSize' 256, which seems to be a good default.
class Radix a where
    -- | The size of an auxiliary array, i.e. the counting bucket
    bucketSize :: a -> Int
    -- | The number of passes necessary to sort an array of es,
    --   it equals to the key's byte number.
    passes :: a -> Int
    -- | The radix function used in the first pass, works on the least significant bit.
    radixLSB  :: a -> Int
    -- | The radix function parameterized by the current pass (0 < pass < passes e-1).
    radix  :: Int -> a -> Int
    -- | The radix function used in the last pass, works on the most significant bit.
    radixMSB  :: a -> Int

instance Radix Int8 where
    {-# INLINE bucketSize #-};
    bucketSize _ = 256
    {-# INLINE passes #-}
    passes _ = 1
    {-# INLINE radixLSB #-}
    radixLSB a =  255 .&. fromIntegral a `xor` 128
    {-# INLINE radix #-}
    radix _ a =  255 .&. fromIntegral a `xor` 128
    {-# INLINE radixMSB #-}
    radixMSB a =  255 .&. fromIntegral a `xor` 128

#define MULTI_BYTES_INT_RADIX(T) \
    {-# INLINE bucketSize #-}; \
    bucketSize _ = 256; \
    {-# INLINE passes #-}; \
    passes _ = sizeOf (undefined :: T); \
    {-# INLINE radixLSB #-}; \
    radixLSB a = fromIntegral (255 .&. a); \
    {-# INLINE radix #-}; \
    radix i a = fromIntegral (a `unsafeShiftR` (i `unsafeShiftL` 3)) .&. 255; \
    {-# INLINE radixMSB #-}; \
    radixMSB a = fromIntegral ((a `xor` minBound) `unsafeShiftR` ((passes a-1) `unsafeShiftL` 3)) .&. 255

instance Radix Int where MULTI_BYTES_INT_RADIX(Int)
instance Radix Int16 where MULTI_BYTES_INT_RADIX(Int16)
instance Radix Int32 where MULTI_BYTES_INT_RADIX(Int32)
instance Radix Int64 where MULTI_BYTES_INT_RADIX(Int64)

instance Radix Word8 where
    {-# INLINE bucketSize #-};
    bucketSize _ = 256
    {-# INLINE passes #-}
    passes _ = 1
    {-# INLINE radixLSB #-}
    radixLSB = fromIntegral
    {-# INLINE radix #-}
    radix _  = fromIntegral
    {-# INLINE radixMSB #-}
    radixMSB = fromIntegral

#define MULTI_BYTES_WORD_RADIX(T) \
    {-# INLINE bucketSize #-}; \
    bucketSize _ = 256; \
    {-# INLINE passes #-}; \
    passes _ = sizeOf (undefined :: T); \
    {-# INLINE radixLSB #-}; \
    radixLSB a = fromIntegral (255 .&. a); \
    {-# INLINE radix #-}; \
    radix i a = fromIntegral (a `unsafeShiftR` (i `unsafeShiftL` 3)) .&. 255; \
    {-# INLINE radixMSB #-}; \
    radixMSB a = fromIntegral (a `unsafeShiftR` ((passes a-1) `unsafeShiftL` 3)) .&. 255

instance Radix Word where MULTI_BYTES_INT_RADIX(Word)
instance Radix Word16 where MULTI_BYTES_INT_RADIX(Word16)
instance Radix Word32 where MULTI_BYTES_INT_RADIX(Word32)
instance Radix Word64 where MULTI_BYTES_INT_RADIX(Word64)

-- | Similar to 'Down' newtype for 'Ord', this newtype can inverse the order of a 'Radix'
-- instance when used in 'radixSort'.
newtype RadixDown a = RadixDown a deriving (Show, Eq, Prim)

instance Radix a => Radix (RadixDown a) where
    {-# INLINE bucketSize #-}
    bucketSize (RadixDown a) = bucketSize a
    {-# INLINE passes #-}
    passes (RadixDown a)  = passes a
    {-# INLINE radixLSB #-}
    radixLSB (RadixDown a) = bucketSize a - radixLSB a -1
    {-# INLINE radix #-}
    radix i (RadixDown a) = bucketSize a - radix i a -1
    {-# INLINE radixMSB #-}
    radixMSB (RadixDown a) = bucketSize a - radixMSB a -1

-- | /O(n)/ Sort vector based on element's 'Radix' instance with
-- <https://en.wikipedia.org/wiki/Radix_sort radix-sort>,
-- (Least significant digit radix sorts variation).
--
-- This is a stable sort, one or two extra O(n) worker array are need
-- depend on how many 'passes' shall be performed, and a 'bucketSize'
-- counting bucket are also needed. This sort algorithms performed extremly
-- well on small byte size types such as 'Int8' or 'Word8', while on larger
-- type, constant passes may render this algorithm not suitable for small
-- vectors (turning point around 2^(2*passes)).
radixSort :: forall v a. (Vec v a, Radix a) => v a -> v a
{-# INLINABLE radixSort #-}
radixSort v@(Vec arr s l)
    | l <= 1 = v
    | otherwise = runST (do
        bucket <- newArrWith buktSiz 0 :: ST s (MutablePrimArray s Int)
        w1 <- newArr l
        firstCountPass arr bucket s
        accumBucket bucket buktSiz 0 0
        firstMovePass arr s bucket w1
        w <- if passSiz == 1
            then unsafeFreezeArr w1
            else do
                w2 <- newArr l
                radixLoop w1 w2 bucket buktSiz 1
        return $! fromArr w 0 l)
  where
    passSiz = passes (undefined :: a)
    buktSiz = bucketSize (undefined :: a)
    !end = s + l

    {-# INLINABLE firstCountPass #-}
    firstCountPass !arr' !bucket !i
        | i >= end  = return ()
        | otherwise = case indexArr' arr' i of
            (# x #) -> do
                let !r = radixLSB x
                c <- readArr bucket r
                writeArr bucket r (c+1)
                firstCountPass arr' bucket (i+1)

    {-# INLINABLE accumBucket #-}
    accumBucket !bucket !bsiz !i !acc
        | i >= bsiz = return ()
        | otherwise = do
            c <- readArr bucket i
            writeArr bucket i acc
            accumBucket bucket bsiz (i+1) (acc+c)

    {-# INLINABLE firstMovePass #-}
    firstMovePass !arr' !i !bucket !w
        | i >= end  = return ()
        | otherwise = case indexArr' arr' i of
            (# x #) -> do
                let !r = radixLSB x
                c <- readArr bucket r
                writeArr bucket r (c+1)
                writeArr w c x
                firstMovePass arr' (i+1) bucket w

    {-# INLINABLE radixLoop #-}
    radixLoop !w1 !w2 !bucket !bsiz !pass
        | pass >= passSiz-1 = do
            setArr bucket 0 bsiz 0   -- clear the counting bucket
            lastCountPass w1 bucket 0
            accumBucket bucket bsiz 0 0
            lastMovePass w1 bucket w2 0
            unsafeFreezeArr w2
        | otherwise = do
            setArr bucket 0 bsiz 0   -- clear the counting bucket
            countPass w1 bucket pass 0
            accumBucket bucket bsiz 0 0
            movePass w1 bucket pass w2 0
            radixLoop w2 w1 bucket bsiz (pass+1)

    {-# INLINABLE countPass #-}
    countPass !marr !bucket !pass !i
        | i >= l  = return ()
        | otherwise = do
                x <- readArr marr i
                let !r = radix pass x
                c <- readArr bucket r
                writeArr bucket r (c+1)
                countPass marr bucket pass (i+1)

    {-# INLINABLE movePass #-}
    movePass !src !bucket !pass !target !i
        | i >= l  = return ()
        | otherwise = do
                x <- readArr src i
                let !r = radix pass x
                c <- readArr bucket r
                writeArr bucket r (c+1)
                writeArr target c x
                movePass src bucket pass target (i+1)

    {-# INLINABLE lastCountPass #-}
    lastCountPass !marr !bucket !i
        | i >= l  = return ()
        | otherwise = do
                x <- readArr marr i
                let !r = radixMSB x
                c <- readArr bucket r
                writeArr bucket r (c+1)
                lastCountPass marr bucket (i+1)

    {-# INLINABLE lastMovePass #-}
    lastMovePass !src !bucket !target !i
        | i >= l  = return ()
        | otherwise = do
                x <- readArr src i
                let !r = radixMSB x
                c <- readArr bucket r
                writeArr bucket r (c+1)
                writeArr target c x
                lastMovePass src bucket target (i+1)

{- In fact IEEE float can be radix sorted like following:

newtype RadixDouble = RadixDouble Int64 deriving (Show, Eq, Prim)
instance Cast RadixDouble Double where cast (RadixDouble a) = cast a
instance Cast Double RadixDouble where cast a = RadixDouble (cast a)
instance Radix RadixDouble where
    {-# INLINE bucketSize #-}
    bucketSize (RadixDouble _) = 256
    {-# INLINE passes #-}
    passes (RadixDouble _)  = 8
    {-# INLINE radixLSB #-}
    radixLSB (RadixDouble a) | a > 0 = r
                             | otherwise = 255 - r
      where r = radixLSB a
    {-# INLINE radix #-}
    radix i (RadixDouble a) | a > 0 = r
                            | otherwise = 255 - r
      where r = radix i a
    {-# INLINE radixMSB #-}
    radixMSB (RadixDouble a) | r < 128  = r + 128
                             | otherwise = 255 - r
      where r = radixMSB (fromIntegral a :: Word64)

radixSortDouble :: PrimVector Double -> PrimVector Double
radixSortDouble v =  castVector (radixSort (castVector v :: PrimVector RadixDouble))

newtype RadixFloat = RadixFloat Int32 deriving (Show, Eq, Prim)
instance Cast RadixFloat Float where cast (RadixFloat a) = cast a
instance Cast Float RadixFloat where cast a = RadixFloat (cast a)
instance Radix RadixFloat where
    {-# INLINE bucketSize #-}
    bucketSize (RadixFloat _) = 256
    {-# INLINE passes #-}
    passes (RadixFloat _)  = 4
    {-# INLINE radixLSB #-}
    radixLSB (RadixFloat a) | a > 0 = r
                            | otherwise = 255 - r
      where r = radixLSB a
    {-# INLINE radix #-}
    radix i (RadixFloat a) | a > 0 = r
                           | otherwise = 255 - r
      where r = radix i a
    {-# INLINE radixMSB #-}
    radixMSB (RadixFloat a) | r < 128  = r + 128
                            | otherwise = 255 - r
      where r = radixMSB (fromIntegral a :: Word32)

radixSortFloat :: PrimVector Float -> PrimVector Float
radixSortFloat v =  castVector (radixSort (castVector v :: PrimVector RadixFloat))
-}

--------------------------------------------------------------------------------
-- | merge duplicated adjacent element, prefer left element.
--
-- Use this function on a sorted vector will have the same effects as 'nub'.
mergeDupAdjacent :: (Vec v a, Eq a) => v a -> v a
{-# INLINE mergeDupAdjacent #-}
mergeDupAdjacent = mergeDupAdjacentBy (==) const

-- | Merge duplicated adjacent element, prefer left element.
mergeDupAdjacentLeft :: Vec v a
                     => (a -> a -> Bool)   -- ^ equality tester, @\ left right -> eq left right@
                     -> v a
                     -> v a
mergeDupAdjacentLeft eq = mergeDupAdjacentBy eq const
{-# INLINE mergeDupAdjacentLeft #-}

-- | Merge duplicated adjacent element, prefer right element.
mergeDupAdjacentRight :: Vec v a
                      => (a -> a -> Bool)  -- ^ equality tester, @\ left right -> eq left right@
                      -> v a
                      -> v a
{-# INLINE mergeDupAdjacentRight #-}
mergeDupAdjacentRight eq = mergeDupAdjacentBy eq (\ _ x -> x)

-- | Merge duplicated adjacent element, based on a equality tester and a merger function.
mergeDupAdjacentBy :: Vec v a
                   => (a -> a -> Bool)  -- ^ equality tester, @\ left right -> eq left right@
                   -> (a -> a -> a)     -- ^ the merger, @\ left right -> merge left right@
                   -> v a -> v a
{-# INLINABLE mergeDupAdjacentBy #-}
mergeDupAdjacentBy eq merger v@(Vec arr s l)
    | l == 0 = empty
    | l == 1 = v
    | otherwise = createN l $ \ marr -> do
        x0 <- indexArrM arr 0
        writeArr marr 0 x0
        go arr marr s 1 x0
  where
    !end = s + l
    go !arr' !marr !i !j !x
        | i >= end  = return j
        | otherwise = do
            x' <- indexArrM arr' i
            if x `eq` x'
            then do
                let !x'' = merger x x'
                writeArr marr (j-1) x''
                go arr' marr (i+1) j x''
            else do
                writeArr marr j x'
                go arr' marr (i+1) (j+1) x'
