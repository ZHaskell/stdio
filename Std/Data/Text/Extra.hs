{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
Module      : Std.Data.Text.Extra
Description : Fast boxed and unboxed vector
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

Various combinators works on 'Vec' class instances.

-}

module Std.Data.Text.Extra (
  -- * Slice manipulation
    cons, snoc
  , uncons, unsnoc
  , headMaybe, tailMayEmpty
  , lastMaybe, initMayEmpty
  , inits, tails
  , take, drop, takeLast, dropLast
  , slice, (/../)
  , splitAt
  , takeWhile, takeWhileEnd, dropWhile, dropWhileEnd, dropAround
  , break, span
  , breakEnd, spanEnd, breakOn
  , group, groupBy
  , stripPrefix, stripSuffix
  , split, splitWith, splitOn
  , isPrefixOf, isSuffixOf, isInfixOf
  , commonPrefixes
  , words, lines, unwords, unlines
  , padLeft, padRight
  -- * Transform
  , reverse
  , intersperse
  , intercalate
  , intercalateElem
  , transpose
  -- * Zipping
  , zipWith', unzipWith'
  -- * Scans
  , scanl', scanl1'
  , scanr', scanr1'
  ) where

--------------------------------------------------------------------------------
-- Transform

-- | /O(n)/ The 'intersperse' function takes a character and places it
-- between the characters of a 'Text'. Performs replacement on invalid scalar values.
--
intersperse :: Char -> Text -> Text
{-# INLINE intersperse #-}
intersperse c = \ t@(Text (V.PrimVector ba s l)) ->
    let tlen = length t
    in if length t < 2
    then t
    else (runST (do
            mbaC <- newPrimArray 4 -- encoded char buf
            clen <- encodeChar mbaC 0 c
            shrinkMutablePrimArray mbaC clen
            baC <- unsafeFreezePrimArray mbaC
            let e = decodeCharLenReverse ba (s+l-1)
            return . Text $ V.create (l + (tlen-1) * clen) (go baC ba s 0 (s+l-e))
        ))
  where
    go :: PrimArray Word8  -- the encode char buf
       -> PrimArray Word8  -- the original text
       -> Int              -- decoding index of original text
       -> Int              -- writing index of new buf
       -> Int              -- the end of decoding index
       -> MutablePrimArray s Word8 -- the new buf
       -> ST s ()
    go !baC !ba !i !j !end !mba
        | i >= end = do
            let l = decodeCharLen ba i
            copyChar l mba j ba i
        | otherwise = do
            let l = decodeCharLen ba i
            copyChar l mba j ba i
            let i' = i + l
                j' = j + l
            let clen = sizeofArr baC
            copyChar clen mba j' baC 0
            go baC ba i' (j'+clen) end mba

-- | /O(n)/ Reverse the characters of a string.
reverse :: Text -> Text
reverse = \ (Text (V.PrimVector ba s l)) -> Text $ V.create l (go ba s l (s+l))
  where
    go :: PrimArray Word8 -> Int -> Int -> Int -> MutablePrimArray s Word8 -> ST s ()
    go !ba !i !j !end !mba
        | i >= end = return ()
        | otherwise = do
            let l = decodeCharLen ba i
                j' = j - l
            copyChar l mba j' ba i
            go ba (i+l) j' end mba
{-# INLINE reverse #-}

(/../) :: Text -> (Int, Int) -> Text
(/../) = undefined
