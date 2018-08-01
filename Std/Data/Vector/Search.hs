{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-|
Module      : Std.Data.Vector.Search
Description : Searching vectors
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

-}

module Std.Data.Vector.Search (
  -- * Search in vectors
  -- ** element-wise search
    findIndices, elemIndices, elemIndicesBytes
  , find, findIndexOrEnd, elemIndexOrEndBytes
  , findIndexReverseOrStart, elemIndexReverseOrStartBytes
  , filter, partition
  -- ** sub-vector search
  , indicesOverlapping
  , indicesOverlappingBytes
  , indices
  , indicesBytes
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
--
-- To leverage @memchr(3)@, following rewrite rules are included:
--
--   * @findIndices (w `eqWord8`) = elemIndicesBytes@
--   * @findIndices (`eqWord8` w) = elemIndicesBytes@
findIndices :: Vec v a => (a -> Bool) -> v a -> [Int]
{-# INLINE [1] findIndices #-}
{-# RULES "findIndices/Bytes" forall w. findIndices (w `eqWord8`) = elemIndicesBytes w #-}
{-# RULES "findIndices/Bytes" forall w. findIndices (`eqWord8` w) = elemIndicesBytes w #-}
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

-- | /O(n)/ find the first index and element matching the predicate
-- in a vector from left to right, if there isn't one, return 'Nothing'.
find :: Vec v a => (a -> Bool) -> v a -> Maybe (Int, a)
{-# INLINE find #-}
find f v@(Vec arr s l) = go s
  where
    !end = s + l
    go !p | p >= end  = Nothing
          | f x       = let !i = p-s in Just (i, x)
          | otherwise = go (p+1)
        where (# x #) = indexArr' arr p

-- | /O(n)/ find the first index matching the predicate in a vector
-- from left to right, if there isn't one, return the length of the vector,
-- which is not a legal index for the vector!
--
-- To leverage @memchr(3)@, following rewrite rules are included:
--
--   * @findIndexOrEnd (w `eqWord8`) = elemIndexOrEndBytes@
--   * @findIndexOrEnd (`eqWord8` w) = elemIndexOrEndBytes@
findIndexOrEnd :: Vec v a => (a -> Bool) -> v a -> Int
{-# INLINE [1] findIndexOrEnd #-}
{-# RULES "findIndexOrEnd/Bytes" forall w. findIndexOrEnd (w `eqWord8`) = elemIndexOrEndBytes w #-}
{-# RULES "findIndexOrEnd/Bytes" forall w. findIndexOrEnd (`eqWord8` w) = elemIndexOrEndBytes w #-}
findIndexOrEnd f (Vec arr s l) = go s
  where
    !end = s + l
    go !p | p >= end  = l
          | f x       = p-s
          | otherwise = go (p+1)
        where (# x #) = indexArr' arr p

-- | /O(n)/ Special 'elemIndexOrEndBytes' for 'Bytes' using @memchr(3)@
--
--  return l (then length of the vector) if not find target.
elemIndexOrEndBytes :: Word8 -> Bytes -> Int
{-# INLINE elemIndexOrEndBytes #-}
elemIndexOrEndBytes w (PrimVector (PrimArray ba#) s l) =
    case c_memchr ba# s w l of
        -1 -> l
        r  -> r

-- | /O(n)/ find the first index matching the predicate in a vector
-- from right to left, if there isn't one, return -1.
--
-- To leverage bit twidding, following rewrite rules are included:
--
--   * @findIndexReverseOrStart (w `eqWord8`) = elemIndexReverseOrStartBytes@
--   * @findIndexReverseOrStart (`eqWord8` w) = elemIndexReverseOrStartBytes@
findIndexReverseOrStart :: Vec v a => (a -> Bool) -> v a -> Int
{-# INLINE [1] findIndexReverseOrStart #-}
{-# RULES
"findIndexReverseOrStart/Bytes"
    forall w. findIndexReverseOrStart (w `eqWord8`) = elemIndexReverseOrStartBytes w
    #-}
{-# RULES "findIndexReverseOrStart/Bytes"
    forall w. findIndexReverseOrStart (`eqWord8` w) = elemIndexReverseOrStartBytes w
    #-}
findIndexReverseOrStart f (Vec arr s l) = go (s+l-1)
  where
    go !p | p < s     = -1
          | f x       = p-s
          | otherwise = go (p-1)
        where (# x #) = indexArr' arr p

-- | /O(n)/ Special 'elemIndexReverseOrStartBytes' for 'Bytes' with handle roll
-- bit twiddling.
elemIndexReverseOrStartBytes :: Word8 -> Bytes -> Int
{-# INLINE elemIndexReverseOrStartBytes #-}
elemIndexReverseOrStartBytes w (PrimVector ba s l) = memchrReverse ba w (s+l-1) l

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
-- References:
--
--  * Knuth, Donald; Morris, James H.; Pratt, Vaughan: "Fast pattern matching in strings" (1977)
--  * <http://www-igm.univ-mlv.fr/~lecroq/string/node8.html#SECTION0080>
indicesOverlapping :: (Vec v a, Eq a)
        => v a -- ^ vector to search for (@needle@)
        -> v a -- ^ vector to search in (@haystack@)
        -> Bool -- ^ report partial match at the end of haystack
        -> [Int]
{-# INLINE[1] indicesOverlapping #-}
{-# RULES "indicesOverlapping/Bytes" indicesOverlapping = indicesOverlappingBytes #-}
indicesOverlapping needle@(Vec narr noff nlen) haystack@(Vec harr hoff hlen) reportPartial
    | nlen == 1             = case indexArr' narr 0 of
                                (# x #) -> elemIndices x haystack
    | nlen <= 0 || hlen < 0 = []
    | otherwise             = kmp 0 0
  where
    {-# NOINLINE next #-} -- force sharing
    next = kmpNextTable needle
    kmp !i !j | i >= hlen = if j >= nlen then [i-j]
                            else if reportPartial && j /= 0 then [-j] else []
              | j >= nlen = let !i' = i-j
                            in case next `indexArr` j of
                                -1 -> i' : kmp i 0
                                j' -> i' : kmp i j'
              | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) = kmp (i+1) (j+1)
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
-- A rewrite rule to rewrite 'indicesOverlapping' to 'indicesOverlappingBytes' is
-- also included.
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
{-# INLINE indicesOverlappingBytes #-}
indicesOverlappingBytes needle@(Vec narr noff nlen) haystack@(Vec harr hoff hlen) reportPartial
    | nlen == 1             = elemIndices (indexArr narr 0) haystack
    | nlen <= 0 || hlen < 0 = []
    | otherwise             = sunday 0 0
  where
    {-# NOINLINE next #-} -- force sharing
    next = kmpNextTable needle
    {-# NOINLINE bloom #-} -- force sharing
    bloom = sundayBloom needle
    kmp !i !j | i >= hlen = if j >= nlen then [i-j]
                            else if reportPartial && j /= 0 then [-j] else []
              | j >= nlen = let !i' = i-j
                            in case next `indexArr` j of
                                -1 -> i' : kmp i 0
                                j' -> i' : kmp i j'
              | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) = kmp (i+1) (j+1)
              | otherwise = case next `indexArr` j of
                                -1 -> kmp (i+1) 0
                                j' -> kmp i j'

    !hlen' = hlen - nlen
    sunday !i !j | i >= hlen' = kmp i j
                 | j >= nlen  = let !i' = i-j
                                in case next `indexArr` j of
                                    -1 -> i' : sunday i 0
                                    j' -> i' : sunday i j'
                 | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) = sunday (i+1) (j+1)
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
indices :: (Vec v a, Eq a) => v a -> v a -> Bool -> [Int]
{-# INLINE[1] indices #-}
{-# RULES "indices/Bytes" indices = indicesBytes #-}
indices needle@(Vec narr noff nlen) haystack@(Vec harr hoff hlen) reportPartial
    | nlen == 1             = case indexArr' narr 0 of
                                (# x #) -> elemIndices x haystack
    | nlen <= 0 || hlen < 0 = []
    | otherwise             = kmp 0 0
  where
    {-# NOINLINE next #-} -- force sharing
    next = kmpNextTable needle
    kmp !i !j | i >= hlen = if j >= nlen then [i-j]
                            else if reportPartial && j /= 0 then [-j] else []
                | j >= nlen = let !i' = i-j in i' : kmp i 0
                | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) = kmp (i+1) (j+1)
                | otherwise = case next `indexArr` j of
                                -1 -> kmp (i+1) 0
                                j' -> kmp i j'

-- | /O(n\/m)/ Find the offsets of all non-overlapping indices of @needle@
-- within @haystack@ using KMP algorithm, combined with simplified sunday's
-- rule to obtain /O(m\/n)/ complexity in average use case.
--
-- A rewrite rule to rewrite 'indices' to 'indicesBytes' is
-- also included.
indicesBytes :: Bytes -- ^ bytes to search for (@needle@)
             -> Bytes -- ^ bytes to search in (@haystack@)
             -> Bool -- ^ report partial match at the end of haystack
             -> [Int]
{-# INLINE indicesBytes #-}
indicesBytes needle@(Vec narr noff nlen) haystack@(Vec harr hoff hlen) reportPartial
    | nlen == 1             = elemIndices (indexArr narr 0) haystack
    | nlen <= 0 || hlen < 0 = []
    | otherwise             = sunday 0 0
  where
    {-# NOINLINE next #-} -- force sharing
    next = kmpNextTable needle
    {-# NOINLINE bloom #-} -- force sharing
    bloom = sundayBloom needle
    kmp !i !j | i >= hlen = if j >= nlen then [i-j]
                            else if reportPartial && j /= 0 then [-j] else []
              | j >= nlen = let !i' = i-j in i' : kmp i 0
              | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) = kmp (i+1) (j+1)
              | otherwise = case next `indexArr` j of
                                -1 -> kmp (i+1) 0
                                j' -> kmp i j'

    !hlen' = hlen - nlen
    sunday !i !j | i >= hlen' = kmp i j
                 | j >= nlen  = let !i' = i-j in  i' : sunday i 0
                 | narr `indexArr` (j+noff) == harr `indexArr` (i+hoff) = sunday (i+1) (j+1)
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
{-# INLINABLE kmpNextTable #-}
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
{-# INLINABLE sundayBloom #-}
sundayBloom (Vec arr s l) = go 0x00 s
  where
    !end = s+l
    go !b !i
        | i >= end  = b
        | otherwise =
            let !w = indexArr arr i
                !b' = b .|. (0x01 `unsafeShiftL` (fromIntegral w .&. 0x3f))
            in go b' (i+1)

-- | O(1) Test if a bloom filter contain a certain 'Word8'.
--
elemSundayBloom :: Word64 -> Word8 -> Bool
{-# INLINE elemSundayBloom #-}
elemSundayBloom b w = b .&. (0x01 `unsafeShiftL` (fromIntegral w .&. 0x3f)) /= 0
