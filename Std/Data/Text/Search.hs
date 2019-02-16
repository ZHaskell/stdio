{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}


{-|
Module      : Std.Data.Text.Search
Description : Searching text
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

-}

module Std.Data.Text.Search (
  -- * element-wise search
    elem, notElem
  -- * Searching by equality
  , findIndices
  , find, findR
  , findIndex
  , findIndexR
  , filter, partition
  ) where


import           Control.Monad.ST
import           Data.Word
import           Prelude                 hiding (elem, notElem, filter, partition)
import           Std.Data.Array
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

-- | /O(n)/ find the first char matching the predicate in a text
-- from left to right, if there isn't one, return the index point to the end of the byte slice.
find :: (Char -> Bool)
     -> Text
     -> (Int, Int, Maybe Char)  -- ^ (char index, byte index, matching char)
{-# INLINE find #-}
find f (Text (V.PrimVector arr s l)) = go 0 s
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
findR :: (Char -> Bool) -> Text -> (Int, Int, Maybe Char)
{-# INLINE findR #-}
findR f (Text (V.PrimVector arr s l)) = go 0 (s+l-1)
  where
    go !i !j | j < s     = (i, j, Nothing)
             | otherwise =
                let (# x, off #) = decodeCharReverse arr j
                in if f x
                    then (i, j, Just x)
                    else go (i+1) (j-off)

--------------------------------------------------------------------------------

-- | /O(n)/ find the index of the byte slice.
findIndex :: (Char -> Bool) -> Text -> Int
{-# INLINE findIndex #-}
findIndex f t = case find f t of (_, i, _) -> i

-- | /O(n)/ find the index of the byte slice in reverse order.
findIndexR ::  (Char -> Bool) -> Text -> Int
{-# INLINE findIndexR #-}
findIndexR f t = case findR f t of (_, i, _) -> i

-- | /O(n)/ 'filter', applied to a predicate and a text,
-- returns a text containing those chars that satisfy the
-- predicate.
filter :: (Char -> Bool) -> Text -> Text
{-# INLINE filter #-}
filter f (Text (V.PrimVector arr s l)) = Text (V.createN l (go s 0))
  where
    !end = s + l
    go :: Int -> Int -> MutablePrimArray s Word8 -> ST s Int
    go !i !j marr
        | i >= end = return j
        | otherwise =
            let (# x, off #) = decodeChar arr i
            in if f x
                then do
                    copyChar off marr j arr i
                    go (i+off) (j+off) marr
                else go (i+off) j marr

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
        | otherwise =
            let (# x, off #) = decodeChar arr p
            in if f x
                then copyChar off mba0 i arr p >> go (i+off) j (p+off) mba0 mba1
                else copyChar off mba1 j arr p >> go i (j+off) (p+off) mba0 mba1

--------------------------------------------------------------------------------
-- Searching by equality

-- | /O(n)/ 'elem' test if given char is in given text.
elem :: Char -> Text -> Bool
{-# INLINE elem #-}
elem x t = case find (x==) t of (_,_,Nothing) -> False
                                _             -> True

-- | /O(n)/ @not . elem@
notElem ::  Char -> Text -> Bool
{-# INLINE notElem #-}
notElem x = not . elem x
