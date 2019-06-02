{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Std.Data.Vector.Search
Description : Searching vectors
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides:

  * Element-wise searching within vectors

  * Fast sub-vector searching algorithm based on KMP string searching.

  * A hybrid sub-vector searching algorithm for 'Bytes'.

  * Rewrite rules to use 'Bytes' specialized version if possible.

-}

module Std.Data.Vector.Search (
  -- * Element-wise search
    findIndices, elemIndices
  , find, findR
  , findIndex, findIndexR
  , filter, partition
  -- * Sub-vector search
  , indicesOverlapping
  , indices
  -- * 'Bytes' specialized combinators
  , elemIndicesBytes, findByte, findByteR
  , indicesOverlappingBytes, indicesBytes
  -- * Helpers
  , kmpNextTable
  , sundayBloom
  , elemSundayBloom
  ) where

import           Control.Monad.ST
import           Data.Bits
import           GHC.Word
import           Prelude                       hiding (filter, partition)
import           Std.Data.Array
import           Std.Data.PrimArray.BitTwiddle (c_memchr, memchrReverse)
import           Std.Data.Vector.Base

--------------------------------------------------------------------------------
-- Searching by equality or predicate

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: (Vec v a, Eq a) => a -> v a -> [Int]
{-# INLINE [1] elemIndices #-}
{-# RULES "elemIndices/Bytes" elemIndices = elemIndicesBytes #-}
elemIndices w (Vec arr s l) = go s
  where
    !end = s + l
    go !i
        | i >= end  = []
        | x == w    = let !i' = i - s in i' : go (i+1)
        | otherwise = go (i+1)
        where (# x #) = indexArr' arr i

-- | The 'findIndex' function takes a predicate and a vector and
-- returns the index of the first element in the vector
-- satisfying the predicate.
findIndices :: Vec v a => (a -> Bool) -> v a -> [Int]
{-# INLINE [1] findIndices #-}
{-# RULES "findIndices/Bytes1" forall w. findIndices (w `eqWord8`) = elemIndicesBytes w #-}
{-# RULES "findIndices/Bytes2" forall w. findIndices (`eqWord8` w) = elemIndicesBytes w #-}
findIndices f (Vec arr s l) = go s
  where
    !end = s + l
    go !p | p >= end  = []
          | f x       = p : go (p+1)
          | otherwise = go (p+1)
        where (# x #) = indexArr' arr p

-- | /O(n)/ Special 'elemIndices' for 'Bytes' using @memchr(3)@
elemIndicesBytes :: Word8 -> Bytes -> [Int]
{-# INLINE elemIndicesBytes #-}
elemIndicesBytes w (PrimVector (PrimArray ba#) s l) = go s
  where
    !end = s + l
    go !i
        | i >= end = []
        | otherwise =
            case c_memchr ba# i w (end - i) of
                -1 -> []
                r  -> let !i' = (i+r) in i': go (i'+1)

-- | @findIndex f v = fst (find f v)@
findIndex :: Vec v a => (a -> Bool) -> v a -> Int
{-# INLINE findIndex #-}
findIndex f v = fst (find f v)

-- | @findIndexR f v = fst (findR f v)@
findIndexR :: Vec v a => (a -> Bool) -> v a -> Int
{-# INLINE findIndexR #-}
findIndexR f v = fst (findR f v)

-- | /O(n)/ find the first index and element matching the predicate in a vector
-- from left to right, if there isn't one, return (length of the vector, Nothing).
--
find :: Vec v a => (a -> Bool) -> v a -> (Int, Maybe a)
{-# INLINE [1] find #-}
{-# RULES "find/Bytes1" forall w. find (w `eqWord8`) = findByte w #-}
{-# RULES "find/Bytes2" forall w. find (`eqWord8` w) = findByte w #-}
find f (Vec arr s l) = go s
  where
    !end = s + l
    go !p | p >= end  = (l, Nothing)
          | f x       = let !i = p-s in (i, Just x)
          | otherwise = go (p+1)
        where (# x #) = indexArr' arr p

-- | /O(n)/ Special 'findByte' for 'Word8' using @memchr(3)@
findByte :: Word8 -> Bytes -> (Int, Maybe Word8)
{-# INLINE findByte #-}
findByte w (PrimVector (PrimArray ba#) s l) =
    case c_memchr ba# s w l of
        -1 -> (l, Nothing)
        r  -> (r, Just w)


-- | /O(n)/ find the first index and element matching the predicate
-- in a vector from right to left, if there isn't one, return '(-1, Nothing)'.
findR :: Vec v a => (a -> Bool) -> v a -> (Int, Maybe a)
{-# INLINE [1] findR #-}
{-# RULES "findR/Bytes1" forall w. findR (w `eqWord8`) = findByteR w #-}
{-# RULES "findR/Bytes2" forall w. findR (`eqWord8` w) = findByteR w #-}
findR f (Vec arr s l) = go (s+l-1)
  where
    go !p | p < s     = (-1, Nothing)
          | f x       = let !i = p-s in (i, Just x)
          | otherwise = go (p-1)
        where (# x #) = indexArr' arr p

-- | /O(n)/ Special 'findR' for 'Bytes' with handle roll bit twiddling.
findByteR :: Word8 -> Bytes -> (Int, Maybe Word8)
{-# INLINE findByteR #-}
findByteR w (PrimVector ba s l) =
    case memchrReverse ba w (s+l-1) l of
        -1 -> (-1, Nothing)
        r  -> (r, Just w)

-- | /O(n)/ 'filter', applied to a predicate and a vector,
-- returns a vector containing those elements that satisfy the
-- predicate.
filter :: forall v a. Vec v a => (a -> Bool) -> v a -> v a
{-# INLINE filter #-}
filter f (Vec arr s l)
    | l == 0    = empty
    | otherwise = createN l (go f 0 s)
  where
    !end = s + l
    go :: (a -> Bool) -> Int -> Int -> MArray v s a -> ST s Int
    go f !i !p !marr
        | p >= end    = return i
        | f x         = writeArr marr i x >> go f (i+1) (p+1) marr
        | otherwise   = go f i (p+1) marr
        where (# x #) = indexArr' arr p

-- | /O(n)/ The 'partition' function takes a predicate, a vector, returns
-- a pair of vector with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p vs == (filter p vs, filter (not . p) vs)
partition :: forall v a. Vec v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE partition #-}
partition f (Vec arr s l)
    | l == 0    = (empty, empty)
    | otherwise = createN2 l l (go f 0 0 s)
  where
    !end = s + l
    go :: (a -> Bool) -> Int -> Int -> Int -> MArray v s a -> MArray v s a -> ST s (Int, Int)
    go f !i !j !p !mba0 !mba1
        | p >= end   = return (i, j)
        | f x        = writeArr mba0 i x >> go f (i+1) j (p+1) mba0 mba1
        | otherwise  = writeArr mba1 j x >> go f i (j+1) (p+1) mba0 mba1
        where (# x #) = indexArr' arr p

--------------------------------------------------------------------------------
-- Sub vector search

-- | /O(n+m)/ Find the offsets of all indices (possibly overlapping) of @needle@
-- within @haystack@ using KMP algorithm.
--
-- The KMP algorithm need pre-calculate a shift table in /O(m)/ time and space,
-- the worst case time complexity is /O(n+m)/. Partial apply this function to
-- reuse pre-calculated table between same needles.
--
-- Chunked input are support via partial match argument, if set we will return an
-- extra negative index in case of partial match at the end of input chunk, e.g.
--
-- > indicesOverlapping [ascii|ada|]  [ascii|adadad|] True == [0,2,-2]
--
-- Where @-2@ is the length of the partial match part @ad@ 's negation.
--
-- If an empty pattern is supplied, we will return every possible index of haystack,
-- e.g.
--
-- > indicesOverlapping "" "abc" = [0,1,2]
--
-- References:
--
--  * Knuth, Donald; Morris, James H.; Pratt, Vaughan: "Fast pattern matching in strings" (1977)
--  * <http://www-igm.univ-mlv.fr/~lecroq/string/node8.html#SECTION0080>
indicesOverlapping :: (Vec v a, Eq a)
        => v a -- ^ vector to search for (@needle@)
        -> v a -- ^ vector to search in (@haystack@)
        -> Bool -- ^ report partial match at the end of haystack
        -> [Int]
{-# INLINABLE[1] indicesOverlapping #-}
{-# RULES "indicesOverlapping/Bytes" indicesOverlapping = indicesOverlappingBytes #-}
indicesOverlapping needle@(Vec narr noff nlen) = search
  where
    next = kmpNextTable needle
    search haystack@(Vec harr hoff hlen) reportPartial
        | nlen <= 0 = [0..hlen-1]
        | nlen == 1 = case indexArr' narr 0 of
                       (# x #) -> elemIndices x haystack
        | otherwise = kmp 0 0
      where
        kmp !i !j | i >= hlen = if reportPartial && j /= 0 then [-j] else []
                  | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) =
                        let !j' = j+1
                        in if j' >= nlen
                        then let !i' = i-j
                            in case next `indexArr` j' of
                                -1 -> i' : kmp (i+1) 0
                                j'' -> i' : kmp (i+1) j''
                        else kmp (i+1) j'
                  | otherwise = case next `indexArr` j of
                                    -1 -> kmp (i+1) 0
                                    j' -> kmp i j'

-- | /O(n\/m)/ Find the offsets of all indices (possibly overlapping) of @needle@
-- within @haystack@ using KMP algorithm, combined with simplified sunday's
-- rule to obtain /O(n\/m)/ complexity in average use case.
--
-- The hybrid algorithm need pre-calculate a shift table in /O(m)/ time and space,
-- and a bad character bloom filter in /O(m)/ time and /O(1)/ space, the worst case
-- time complexity is /O(n+m)/.
--
-- References:
--
-- * Frantisek FranekChristopher G. JenningsWilliam F. Smyth A Simple Fast Hybrid Pattern-Matching Algorithm (2005)
-- * D. M. Sunday: A Very Fast Substring Search Algorithm. Communications of the ACM, 33, 8, 132-142 (1990)
-- * F. Lundh: The Fast Search Algorithm. <http://effbot.org/zone/stringlib.htm> (2006)
indicesOverlappingBytes :: Bytes -- ^ bytes to search for (@needle@)
                        -> Bytes -- ^ bytes to search in (@haystack@)
                        -> Bool -- ^ report partial match at the end of haystack
                        -> [Int]
{-# INLINABLE indicesOverlappingBytes #-}
indicesOverlappingBytes needle@(Vec narr noff nlen) | popCount bloom > 48 = search
                                                    | otherwise = search'
  where
    next = kmpNextTable needle
    bloom = sundayBloom needle
    search haystack@(Vec harr hoff hlen) reportPartial
        | nlen <= 0 = [0..hlen-1]
        | nlen == 1 = case indexArr' narr 0 of
                       (# x #) -> elemIndices x haystack
        | otherwise = kmp 0 0
      where
        kmp !i !j | i >= hlen = if reportPartial && j /= 0 then [-j] else []
                  | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) =
                        let !j' = j+1
                        in if j' >= nlen
                        then let !i' = i-j
                            in case next `indexArr` j' of
                                -1 -> i' : kmp (i+1) 0
                                j'' -> i' : kmp (i+1) j''
                        else kmp (i+1) j'
                  | otherwise = case next `indexArr` j of
                                    -1 -> kmp (i+1) 0
                                    j' -> kmp i j'
    search' haystack@(Vec harr hoff hlen) reportPartial
        | nlen <= 0 = [0..hlen-1]
        | nlen == 1 = elemIndices (indexArr narr 0) haystack
        | otherwise = sunday 0 0
      where
        kmp !i !j | i >= hlen = if reportPartial && j /= 0 then [-j] else []
                  | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) =
                        let !j' = j+1
                        in if j' >= nlen
                        then let !i' = i-j
                            in case next `indexArr` j' of
                                -1 -> i' : kmp (i+1) 0
                                j'' -> i' : kmp (i+1) j''
                        else kmp (i+1) j'
                  | otherwise = case next `indexArr` j of
                                    -1 -> kmp (i+1) 0
                                    j' -> kmp i j'
        !hlen' = hlen - nlen
        sunday !i !j | i >= hlen' = kmp i j
                     | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) =
                            let !j' = j+1
                            in if j' >= nlen
                            then let !i' = i-j
                                in case next `indexArr` j' of
                                    -1 -> i' : sunday (i+1) 0
                                    j'' -> i' : sunday (i+1) j''
                            else sunday (i+1) j'
                     | otherwise = let !k = i+nlen-j
                                       !afterNeedle = indexArr harr (k+hoff)
                                   in if elemSundayBloom bloom afterNeedle
                                      -- fallback to KMP
                                      then case next `indexArr` j of
                                               -1 -> sunday (i+1) 0
                                               j' -> sunday i j'
                                      -- sunday's shifting
                                      else sunday (k+1) 0

-- | /O(n+m)/ Find the offsets of all non-overlapping indices of @needle@
-- within @haystack@ using KMP algorithm.
--
-- If an empty pattern is supplied, we will return every possible index of haystack,
-- e.g.
--
-- > indicesOverlapping "" "abc" = [0,1,2]
indices :: (Vec v a, Eq a) => v a -> v a -> Bool -> [Int]
{-# INLINABLE[1] indices #-}
{-# RULES "indices/Bytes" indices = indicesBytes #-}
indices needle@(Vec narr noff nlen) = search
  where
    next = kmpNextTable needle
    search haystack@(Vec harr hoff hlen) reportPartial
        | nlen <= 0 = [0..hlen-1]
        | nlen == 1 = case indexArr' narr 0 of
                       (# x #) -> elemIndices x haystack
        | otherwise = kmp 0 0
      where
        kmp !i !j | i >= hlen = if reportPartial && j /= 0 then [-j] else []
                  | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) =
                        let !j' = j+1
                        in if j' >= nlen
                            then let !i' = i-j in i' : kmp (i+1) 0
                            else kmp (i+1) j'
                  | otherwise = case next `indexArr` j of
                                    -1 -> kmp (i+1) 0
                                    j' -> kmp i j'

-- | /O(n\/m)/ Find the offsets of all non-overlapping indices of @needle@
-- within @haystack@ using KMP algorithm, combined with simplified sunday's
-- rule to obtain /O(m\/n)/ complexity in average use case.
indicesBytes :: Bytes -- ^ bytes to search for (@needle@)
             -> Bytes -- ^ bytes to search in (@haystack@)
             -> Bool -- ^ report partial match at the end of haystack
             -> [Int]
{-# INLINABLE indicesBytes #-}
indicesBytes needle@(Vec narr noff nlen) | popCount bloom > 48 = search
                                         | otherwise = search'
  where
    next = kmpNextTable needle
    bloom = sundayBloom needle
    search haystack@(Vec harr hoff hlen) reportPartial
        | nlen <= 0 = [0..hlen-1]
        | nlen == 1 = case indexArr' narr 0 of
                       (# x #) -> elemIndices x haystack
        | otherwise = kmp 0 0
      where
        kmp !i !j | i >= hlen = if reportPartial && j /= 0 then [-j] else []
                  | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) =
                        let !j' = j+1
                        in if j' >= nlen
                            then let !i' = i-j in i' : kmp (i+1) 0
                            else kmp (i+1) j'
                  | otherwise = case next `indexArr` j of
                                    -1 -> kmp (i+1) 0
                                    j' -> kmp i j'
    search' haystack@(Vec harr hoff hlen) reportPartial
        | nlen <= 0 = [0..hlen-1]
        | nlen == 1 = elemIndices (indexArr narr 0) haystack
        | otherwise = sunday 0 0
      where
        kmp !i !j | i >= hlen = if reportPartial && j /= 0 then [-j] else []
                  | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) =
                        let !j' = j+1
                        in if j' >= nlen
                            then let !i' = i-j in i' : kmp (i+1) 0
                            else kmp (i+1) j'
                  | otherwise = case next `indexArr` j of
                                    -1 -> kmp (i+1) 0
                                    j' -> kmp i j'
        !hlen' = hlen - nlen
        sunday !i !j | i >= hlen' = kmp i j
                     | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) =
                            let !j' = j+1
                            in if j' >= nlen
                                then let !i' = i-j in i' : sunday (i+1) 0
                                else sunday (i+1) j'
                     | otherwise = let !k = i+nlen-j
                                       !afterNeedle = indexArr harr (k+hoff)
                                   in if elemSundayBloom bloom afterNeedle
                                      -- fallback to KMP
                                      then case next `indexArr` j of
                                               -1 -> sunday (i+1) 0
                                               j' -> sunday i j'
                                      -- sunday's shifting
                                      else sunday (k+1) 0

-- | /O(m)/ Calculate the KMP next shift table.
--
-- The shifting rules is: when a mismatch between @needle[j]@ and @haystack[i]@
-- is found, check if @next[j] == -1@, if so next search continue with @needle[0]@
-- and @haystack[i+1]@, otherwise continue with @needle[next[j]]@ and @haystack[i]@.
kmpNextTable :: (Vec v a, Eq a) => v a -> PrimArray Int
{-# INLINE kmpNextTable #-}
kmpNextTable (Vec arr s l) = runST (do
    ma <- newArr (l+1)
    writeArr ma 0 (-1)
    let dec !w !j
            | j < 0 || w == indexArr arr (s+j) = return $! j+1
            | otherwise = readArr ma j >>= dec w
        go !i !j
            | i > l    = unsafeFreezeArr ma
            | otherwise = do
                let !w = indexArr arr (s+i-1)
                j' <- dec w j
                if i < l && indexArr arr (s+j') == indexArr arr (s+i)
                    then readArr ma j' >>= writeArr ma i
                    else writeArr ma i j'
                go (i+1) j'
    go 1 (-1))

-- | /O(m)/ Calculate a simple bloom filter for simplified sunday's rule.
--
-- The shifting rules is: when a mismatch between @needle[j]@ and @haystack[i]@
-- is found, check if @elemSundayBloom bloom haystack[i+n-j]@, where n is the
-- length of needle, if not then next search can be safely continued with
-- @haystack[i+n-j+1]@ and @needle[0]@, otherwise next searh should continue with
-- @haystack[i]@ and @needle[0]@, or fallback to other shifting rules such as KMP.
--
-- The algorithm is very simple: for a given 'Word8' @w@, we set the bloom's bit
-- at @unsafeShiftL 0x01 (w .&. 0x3f)@, so there're three false positives per bit.
-- This's particularly suitable for search UTF-8 bytes since the significant bits
-- of a beginning byte is usually the same.
sundayBloom :: Bytes -> Word64
{-# INLINE sundayBloom #-}
sundayBloom (Vec arr s l) = go 0x00000000 s
  where
    !end = s+l
    go !b !i
        | i >= end  = b
        | otherwise =
            let !w = indexArr arr i
                !b' = b .|. (0x00000001 `unsafeShiftL` (fromIntegral w .&. 0x3f))
            in go b' (i+1)

-- | O(1) Test if a bloom filter contain a certain 'Word8'.
--
elemSundayBloom :: Word64 -> Word8 -> Bool
{-# INLINE elemSundayBloom #-}
elemSundayBloom b w = b .&. (0x01 `unsafeShiftL` (fromIntegral w .&. 0x3f)) /= 0
