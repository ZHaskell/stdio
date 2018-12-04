{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}


{-|
Module      : Std.Data.Text.Search
Description : Searching text
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

-}

module Std.Data.Text.Search (
  -- * Searching by equality
    elem, notElem
  -- * element-wise search
  , findIndices
  , find, findLast
  , findIndexOrEnd
  , findLastIndexOrStart
  , filter, partition
  -- ** sub-vector search
  , indicesOverlapping
  , indices
  ) where


import           Control.Monad.ST
import           Data.Word
import           Prelude                 hiding (elem, notElem, filter, partition)
import           Std.Data.Array
import           Data.Maybe
import           Std.Data.Text.Base
import           Std.Data.Text.UTF8Codec
import qualified Std.Data.Vector.Base    as V

findIndices :: (Char -> Bool) -> Text -> [Int]
{-# INLINE findIndices #-}
findIndices f (Text (V.PrimVector arr s l)) = go 0 s
  where
    !end = s + l
    go !i !p | p >= end  = []
             | f x       = i : go (i+1) (p+off)
             | otherwise = go (i+1) (p+off)
        where (# x, off #) = decodeChar arr p

find :: (Char -> Bool) -> Text -> Maybe Char
find f t = case findIndexOrEnd f t of (_, _, c) -> c

findLast ::  (Char -> Bool) -> Text -> Maybe Char
findLast f t = case findLastIndexOrStart f t of (_, _, c) -> c

--------------------------------------------------------------------------------

-- | /O(n)/ find the first char matching the predicate in a text
-- from left to right, if there isn't one, return the index point to the end of the byte slice.
findIndexOrEnd :: (Char -> Bool)
               -> Text
               -> (Int, Int, Maybe Char)  -- ^ (char index, byte index, matching char)
{-# INLINE findIndexOrEnd #-}
findIndexOrEnd f (Text (V.PrimVector arr s l)) = go 0 s
  where
    !end = s + l
    go !i !j | j >= end  = (i, j, Nothing)
             | otherwise =
                let (# x, off #) = decodeChar arr j
                in if f x
                    then (i, j, Just x)
                    else go (i+1) (j+off)

-- | /O(n)/ find the first char matching the predicate in a text
-- from right to left, if there isn't one, return the index point to the end of the byte slice.
--
findLastIndexOrStart :: (Char -> Bool) -> Text -> (Int, Int, Maybe Char)
{-# INLINE findLastIndexOrStart #-}
findLastIndexOrStart f (Text (V.PrimVector arr s l)) = go 0 (s+l-1)
  where
    go !i !j | j < s     = (i, j, Nothing)
             | otherwise =
                let (# x, off #) = decodeCharReverse arr j
                in if f x
                    then (i, j, Just x)
                    else go (i+1) (j-off)

filter :: (Char -> Bool) -> Text -> Text
{-# INLINE filter #-}
filter f (Text (V.PrimVector arr s l)) = Text (V.createN l (go s 0))
  where
    !end = s + l
    go :: Int -> Int -> MutablePrimArray s Word8 -> ST s Int
    go !i !j marr
        | i >= end = return j
        | f x  = do
            copyChar off marr j arr i
            go (i+off) (j+off) marr
        | otherwise = go (i+off) j marr
      where (# x, off #) = decodeChar arr j

-- | /O(n)/ The 'partition' function takes a predicate, a text, returns
-- a pair of text with codepoints which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p txt == (filter p txt, filter (not . p) txt)
partition :: (Char -> Bool) -> Text -> (Text, Text)
{-# INLINE partition #-}
partition f (Text (V.PrimVector arr s l))
    | l == 0    = (empty, empty)
    | otherwise = let !(bs1, bs2) = V.createN2 l l (go 0 0 s) in (Text bs1, Text bs2)
  where
    !end = s + l
    go :: Int -> Int -> Int -> MutablePrimArray s Word8 -> MutablePrimArray s Word8 -> ST s (Int, Int)
    go !i !j !p !mba0 !mba1
        | p >= end   = return (i, j)
        | f x        = copyChar off mba0 i arr p >> go (i+off) j (p+off) mba0 mba1
        | otherwise  = copyChar off mba1 j arr p >> go i (j+off) (p+off) mba0 mba1
      where (# x, off #) = decodeChar arr j

--------------------------------------------------------------------------------
-- Searching by equality

-- | /O(n)/ 'elem' test if given char is in given text.
elem :: Char -> Text -> Bool
{-# INLINE elem #-}
elem x = isJust . find (x==)

-- | /O(n)/ 'not . elem'
notElem ::  Char -> Text -> Bool
{-# INLINE notElem #-}
notElem x = not . elem x
