{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Std.Data.Text.Extra
Description : Fast boxed and unboxed vector
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Various combinators works on 'Text's.

-}

module Std.Data.Text.Extra (
  -- * Slice manipulation
    cons, snoc
  , uncons, unsnoc
  , headMaybe, tailMayEmpty
  , lastMaybe, initMayEmpty
  , inits, tails
  , take, drop, takeLast, dropLast
  , slice
  , splitAt
  , takeWhile, takeLastWhile, dropWhile, dropLastWhile, dropAround
  , break, span
  , breakEnd, spanEnd, breakOn
  , breakOnAll, breakOnAllOverlapping
  , group, groupBy
  , stripPrefix, stripSuffix
  , split, splitWith, splitOn
  , isPrefixOf, isSuffixOf, isInfixOf
  , commonPrefix
  , words, lines, unwords, unlines
  , padLeft, padRight
  -- * Transform
  , reverse
  , intersperse
  , intercalate
  , intercalateElem
  , transpose
  ) where

import Data.Primitive.PrimArray
import qualified Std.Data.Vector.Base as V
import qualified Std.Data.Vector.Extra as V
import qualified Std.Data.Vector.Search as V
import Data.Coerce
import qualified Data.List as List
import Std.Data.Text.Base
import Std.Data.Text.UTF8Codec
import Std.Data.Text.Search
import           Control.Monad.ST
import           GHC.Stack
import           Data.Char
import           Data.Word
import           Prelude                       hiding (concat, concatMap,
                                                elem, notElem, null, length, map,
                                                foldl, foldl1, foldr, foldr1,
                                                maximum, minimum, product, sum,
                                                all, any, replicate, traverse,
                                                take, drop, splitAt,
                                                takeWhile, dropWhile,
                                                break, span, reverse,
                                                words, lines, unwords, unlines)


--------------------------------------------------------------------------------
-- Slice manipulation

cons :: Char -> Text -> Text
{-# INLINABLE cons #-}
cons c (Text (V.PrimVector ba s l)) = Text (V.createN (4 + l) (\ mba -> do
    i <- encodeChar mba 0 c
    copyPrimArray mba i ba s l
    return $! i + l))

snoc :: Text -> Char -> Text
{-# INLINABLE snoc #-}
snoc (Text (V.PrimVector ba s l)) c = Text (V.createN (4 + l) (\ mba -> do
    copyPrimArray mba 0 ba s l
    encodeChar mba l c))

uncons :: Text -> Maybe (Char, Text)
{-# INLINE uncons #-}
uncons (Text (V.PrimVector ba s l))
    | l == 0  = Nothing
    | otherwise =
        let (# c, i #) = decodeChar ba s
        in Just (c, Text (V.PrimVector ba (s+i) (l-i)))

unsnoc :: Text -> Maybe (Text, Char)
{-# INLINE unsnoc #-}
unsnoc (Text (V.PrimVector ba s l))
    | l == 0  = Nothing
    | otherwise =
        let (# c, i #) = decodeCharReverse ba (s + l - 1)
        in Just (Text (V.PrimVector ba s (l-i)), c)

headMaybe :: Text -> Maybe Char
{-# INLINABLE headMaybe #-}
headMaybe t = case uncons t of { Just (c, _) -> Just c; _ -> Nothing }

tailMayEmpty :: Text -> Text
{-# INLINABLE tailMayEmpty #-}
tailMayEmpty t = case uncons t of { Nothing -> empty; Just (_, t) -> t }

lastMaybe :: Text -> Maybe Char
{-# INLINABLE lastMaybe #-}
lastMaybe t = case unsnoc t of { Just (_, c) -> Just c; _ -> Nothing }

initMayEmpty :: Text -> Text
{-# INLINABLE initMayEmpty #-}
initMayEmpty t = case unsnoc t of { Just (t, _) -> t; _ -> empty }

inits :: Text -> [Text]
{-# INLINABLE inits #-}
inits t = go t [t]
  where go t acc = case unsnoc t of Just (t', _) -> go t' (t':acc)
                                    Nothing      -> acc

tails :: Text -> [Text]
{-# INLINABLE tails #-}
tails t = t : case uncons t of Just (_, t') -> tails t'
                               Nothing      -> []

take :: Int -> Text -> Text
{-# INLINABLE take #-}
take n t@(Text (V.PrimVector ba s l))
    | n <= 0 = empty
    | otherwise = case charByteIndex t n of i -> Text (V.PrimVector ba s (i-s))

drop :: Int -> Text -> Text
{-# INLINABLE drop #-}
drop n t@(Text (V.PrimVector ba s l))
    | n <= 0 = t
    | otherwise = case charByteIndex t n of i -> Text (V.PrimVector ba i (l+s-i))

takeLast :: Int -> Text -> Text
{-# INLINABLE takeLast #-}
takeLast n t@(Text (V.PrimVector ba s l))
    | n <= 0 = empty
    | otherwise = case charByteIndexR t n of i -> Text (V.PrimVector ba (i+1) (s+l-1-i))

dropLast :: Int -> Text -> Text
{-# INLINABLE dropLast #-}
dropLast n t@(Text (V.PrimVector ba s l))
    | n <= 0 = t
    | otherwise = case charByteIndexR t n of i -> Text (V.PrimVector ba s (i-s+1))

-- | /O(1)/ Extract a sub-range text with give start index and length.
--
-- This function is a total function just like 'take/drop', index/length
-- exceeds range will be ingored, e.g.
--
-- @
-- slice 1 3 "hello"   == "ell"
-- slice -1 -1 "hello" == ""
-- slice -2 2 "hello"  == ""
-- slice 2 10 "hello"  == "llo"
-- @
--
-- This holds for all x y: @slice x y vs == drop x . take (x+y) vs@
slice :: Int -> Int -> Text -> Text
{-# INLINE slice #-}
slice x y t | y <= 0 = empty
            | end <= 0 = empty
            | x <= 0 = take end t
            | otherwise = take y (drop x t)
  where
    !end = x + y

splitAt :: Int -> Text -> (Text, Text)
{-# INLINE splitAt #-}
splitAt n t@(Text (V.PrimVector ba s l))
    | n <= 0 = (empty, t)
    | otherwise = case charByteIndex t n of
        i -> (Text (V.PrimVector ba s (i-s)), Text (V.PrimVector ba i (s+l-i)))


-- | /O(n)/ Applied to a predicate @p@ and a text @t@,
-- returns the longest prefix (possibly empty) of @t@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> Text -> Text
{-# INLINE takeWhile #-}
takeWhile f t@(Text (V.PrimVector arr s l)) =
    let !i = findIndexOrEnd (not . f) t in Text (V.PrimVector arr s (i-s))

-- | /O(n)/ Applied to a predicate @p@ and a text @t@,
-- returns the longest suffix (possibly empty) of @t@ of elements that
-- satisfy @p@.
takeLastWhile :: (Char -> Bool) -> Text -> Text
takeLastWhile f t@(Text (V.PrimVector arr s l)) =
    let !i = findLastIndexOrStart (not . f) t in Text (V.PrimVector arr (i+1) (s+l-i-1))

dropWhile :: (Char -> Bool) -> Text -> Text
{-# INLINE dropWhile #-}
dropWhile f t@(Text (V.PrimVector arr s l)) =
    let !i = findIndexOrEnd (not . f) t in Text (V.PrimVector arr i (s+l-i))

dropLastWhile :: (Char -> Bool) -> Text -> Text
dropLastWhile f t@(Text (V.PrimVector arr s l)) =
    let !i = findLastIndexOrStart (not . f) t in Text (V.PrimVector arr s (i-s+1))

dropAround :: (Char -> Bool) -> Text -> Text
dropAround f = dropLastWhile f . dropWhile f

break :: (Char -> Bool) -> Text -> (Text, Text)
break f t@(Text (V.PrimVector arr s l)) =
    let !i = findIndexOrEnd f t
    in (Text (V.PrimVector arr s (i-s)), Text (V.PrimVector arr i (s+l-i)))

span :: (Char -> Bool) -> Text -> (Text, Text)
span f t@(Text (V.PrimVector arr s l)) =
    let !i = findIndexOrEnd (not . f) t
    in (Text (V.PrimVector arr s (i-s)), Text (V.PrimVector arr i (s+l-i)))

breakEnd :: (Char -> Bool) -> Text -> (Text, Text)
breakEnd f t@(Text (V.PrimVector arr s l)) =
    let !i = findLastIndexOrStart f t
    in (Text (V.PrimVector arr s (i-s+1)), Text (V.PrimVector arr (i+1) (s+l-i-1)))

spanEnd :: (Char -> Bool) -> Text -> (Text, Text)
spanEnd f t@(Text (V.PrimVector arr s l)) =
    let !i = findLastIndexOrStart (not . f) t
    in (Text (V.PrimVector arr s (i-s+1)), Text (V.PrimVector arr (i+1) (s+l-i-1)))

breakOn :: Text -> Text -> (Text, Text)
{-# INLINE breakOn #-}
breakOn (Text needle) (Text haystack) =
    case V.breakOn needle haystack of (v1, v2) -> (Text v1, Text v2)

breakOnAll :: Text -> Text -> [(Text, Text)]
breakOnAll (Text needle) (Text haystack@(V.PrimVector arr s l)) =
    List.map breaker (V.indices needle haystack False)
  where
    breaker i = (Text (V.PrimVector arr s (i-s)), Text (V.PrimVector arr i (s+l-i)))

breakOnAllOverlapping :: Text -> Text -> [(Text, Text)]
breakOnAllOverlapping (Text needle) (Text haystack@(V.PrimVector arr s l)) =
    List.map breaker (V.indicesOverlapping needle haystack False)
  where
    breaker i = (Text (V.PrimVector arr s (i-s)), Text (V.PrimVector arr i (s+l-i)))

group :: Text -> [Text]
{-# INLINE group #-}
group = groupBy (==)

groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
{-# INLINE groupBy #-}
groupBy f (Text (V.PrimVector arr s l))
    | l == 0    = []
    | otherwise = Text (V.PrimVector arr s (s'-s)) : groupBy f (Text (V.PrimVector arr s' (l+s-s')))
  where
    (# c0, s0 #) = decodeChar arr s
    end = s + l
    s' = go arr (s+s0)
    go arr !i
        | i >= end = i
        | otherwise = let (# c1, s1 #) = decodeChar arr i
                      in if f c0 c1 then go arr (i+s1) else i

-- | /O(n)/ The 'stripPrefix' function takes two texts and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
--
stripPrefix :: Text -> Text -> Maybe Text
stripPrefix = coerce (V.stripPrefix @V.PrimVector @Word8)


stripSuffix :: Text -> Text -> Maybe Text
stripSuffix = coerce (V.stripSuffix @V.PrimVector @Word8)

split :: Char -> Text -> [Text]
{-# INLINE split #-}
split x = splitWith (==x)

-- | /O(n)/ Splits a text into components delimited by
-- separators, where the predicate returns True for a separator char.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith (=='a') []        == [""]
--
splitWith :: (Char -> Bool) -> Text -> [Text]
splitWith f (Text (V.PrimVector arr s l)) = go s s
  where
    !end = s + l
    go !p !q | q >= end  = let !v = V.PrimVector arr p (q-p) in [Text v]
             | f c       = let !v = V.PrimVector arr p (q-p) in Text v:go (q+n) (q+n)
             | otherwise = go p (q+n)
        where (# c, n #) = decodeChar arr q


splitOn :: Text -> Text -> [Text]
{-# INLINE splitOn #-}
splitOn = coerce (V.splitOn @V.PrimVector @Word8)

isPrefixOf :: Text -> Text -> Bool
isPrefixOf = coerce (V.isPrefixOf @V.PrimVector @Word8)

isSuffixOf :: Text -> Text -> Bool
isSuffixOf = coerce (V.isSuffixOf @V.PrimVector @Word8)

isInfixOf :: Text -> Text -> Bool
isInfixOf = coerce (V.isInfixOf @V.PrimVector @Word8)

-- | /O(n)/ Find the longest non-empty common prefix of two strings
-- and return it, along with the suffixes of each string at which they
-- no longer match. e.g.
--
-- >>> commonPrefix "foobar" "fooquux"
-- ("foo","bar","quux")
--
-- >>> commonPrefix "veeble" "fetzer"
-- ("","veeble","fetzer")
commonPrefix :: Text -> Text -> (Text, Text, Text)
commonPrefix = coerce (V.commonPrefix @V.PrimVector @Word8)

-- | /O(n)/ Breaks a 'Bytes' up into a list of words, delimited by unicode space.
words ::  Text -> [Text]
words (Text (V.PrimVector arr s l)) = go s s
  where
    !end = s + l
    go !s' !i | i >= end =
                    if s' == end
                    then []
                    else let !v = V.PrimVector arr s' (end-s') in [Text v]
              | otherwise =
                    let (# c, n #) = decodeChar arr i
                    in if isSpace c
                        then if s' == i
                            then go (i+n) (i+n)
                            else let !v = V.PrimVector arr s' (i-s') in Text v : go (i+n) (i+n)
                        else go s' (i+n)

lines :: Text -> [Text]
lines = coerce V.lines

unwords :: [Text] -> Text
unwords = coerce V.unwords

unlines :: [Text] -> Text
unlines = coerce V.unlines

padLeft :: Int -> Char -> Text -> Text
padLeft n c t@(Text (V.PrimVector arr s l))
    | n <= tsiz = t
    | otherwise =
        let psiz = (n-tsiz)*csiz
            siz = psiz + l
        in Text (V.create siz (\ marr -> do
            encodeChar marr 0 c
            go marr csiz psiz
            copyPrimArray marr (siz-l) arr s l))
  where
    tsiz = length t
    csiz = encodeCharLength c
    go marr s psiz
        | s >= psiz = return ()
        | otherwise = copyChar' csiz marr s marr (s-csiz) >> go marr (s+csiz) psiz


padRight :: Int -> Char -> Text -> Text
padRight n c t@(Text (V.PrimVector arr s l))
    | n <= tsiz = t
    | otherwise =
        let psiz = (n-tsiz)*csiz
            siz = psiz + l
        in Text (V.create siz (\ marr -> do
            copyPrimArray marr 0 arr s l
            encodeChar marr l c
            go marr (l+csiz) siz))
  where
    tsiz = length t
    csiz = encodeCharLength c
    go marr s siz
        | s >= siz = return ()
        | otherwise = copyChar' csiz marr s marr (s-csiz) >> go marr (s+csiz) siz


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
            let clen = sizeofPrimArray baC
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

-- | /O(n)/ The 'intercalate' function takes a 'Text' and a list of
-- 'Text's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: Text -> [Text] -> Text
{-# INLINE intercalate #-}
intercalate s = concat . List.intersperse s

intercalateElem :: Char -> [Text] -> Text
intercalateElem c = concat . List.intersperse (singleton c)

-- | The 'transpose' function transposes the rows and columns of its
-- text argument.
--
transpose :: [Text] -> [Text]
{-# INLINE transpose #-}
transpose ts = List.map pack . List.transpose . List.map unpack $ ts

