{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}


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
  -- * Search in vectors
  -- ** element-wise search
    findIndices
  , find, findLast
  , findIndexOrEnd
  , findIndexLastOrStart
  , filter, partition
  -- ** sub-vector search
  , indicesOverlapping
  , indices
  ) where


import Std.Data.Text.Base
import Std.Data.Text.UTF8Codec

findIndices :: (Char -> Bool) -> Text -> [Int]
{-# INLINE findIndices #-}
findIndices f (Text (Vec arr s l)) = go 0 s
  where
    !end = s + l
    go !i !p | p >= end  = []
             | f x       = i : go (i+1) (p+off)
             | otherwise = go (i+1) (p+off)
        where (# x, off #) = decodeChar arr p

find :: (Char -> Bool) -> Text -> Maybe (Int, Char)

findLast ::  (Char -> Bool) -> Text -> Maybe (Int, Char)


--------------------------------------------------------------------------------

-- | /O(n)/ find the first char matching the predicate in a vector
-- from left to right, if there isn't one, return the length of the vector,
findIndexOrEnd :: (Char -> Bool)
               -> Text
               -> (Int, Int, Maybe Char)  -- ^ (char index, byte index, matching char)
findIndexOrEnd (Text (Vec arr s l)) = go 0 s
  where
    !end = s + l
    go !i !p | p >= end  = l
             | f x       = i
             | otherwise = go (i+1) (p+off)
        where (# x, off #) = decodeChar arr p
