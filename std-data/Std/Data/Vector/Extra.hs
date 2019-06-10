{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
Module      : Std.Data.Vector.Extra
Description : Fast vector slice manipulation
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Various combinators works on 'Vec' class instances.

-}

module Std.Data.Vector.Extra (
  -- * Slice manipulation
    cons, snoc
  , uncons, unsnoc
  , headMaybe, tailMayEmpty
  , lastMaybe, initMayEmpty
  , inits, tails
  , take, drop, takeR, dropR
  , slice
  , splitAt
  , takeWhile, takeWhileR, dropWhile, dropWhileR, dropAround
  , break, span, breakR, spanR, breakOn
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
  -- * Zipping
  , zipWith', unzipWith'
  -- * Scans
  , scanl', scanl1'
  , scanr', scanr1'
  -- * Misc
  , rangeCut
  -- * Unsafe operations
  , head
  , tail
  , init
  , last
  , index, indexM
  , unsafeHead
  , unsafeTail
  , unsafeInit
  , unsafeLast
  , unsafeIndex, unsafeIndexM
  , unsafeTake
  , unsafeDrop
  ) where

import           Control.Monad.ST
import           GHC.Stack
import           GHC.Word
import           Std.Data.Array
import           Std.Data.Vector.Base
import           Std.Data.Vector.Search
import           Prelude                       hiding (concat, concatMap,
                                                elem, notElem, null, length, map,
                                                foldl, foldl1, foldr, foldr1,
                                                maximum, minimum, product, sum,
                                                all, any, replicate, traverse,
                                                head, tail, init, last,
                                                take, drop, splitAt,
                                                takeWhile, dropWhile,
                                                break, span, reverse,
                                                words, lines, unwords, unlines)
import qualified Data.List                     as List
import           Control.Exception             (assert)

--------------------------------------------------------------------------------
-- Slice
--
-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
cons :: Vec v a => a -> v a -> v a
{-# INLINE cons #-}
cons x (Vec arr s l) = create (l+1) $ \ marr ->
    writeArr marr 0 x >> copyArr marr 1 arr s l

-- | /O(n)/ Append a byte to the end of a vector
snoc :: Vec v a => v a -> a -> v a
{-# INLINE snoc #-}
snoc (Vec arr s l) x = create (l+1) $ \ marr ->
    copyArr marr 0 arr s l >> writeArr marr l x

-- | /O(1)/ Extract the head and tail of a vector, return 'Nothing'
-- if it is empty.
uncons :: Vec v a => v a -> Maybe (a, v a)
{-# INLINE uncons #-}
uncons (Vec arr s l)
    | l <= 0    = Nothing
    | otherwise = let !v = fromArr arr (s+1) (l-1)
                  in case indexArr' arr s of (# x #) -> Just (x ,v)

-- | /O(1)/ Extract the init and last of a vector, return 'Nothing'
-- if vector is empty.
unsnoc :: Vec v a => v a -> Maybe (v a, a)
{-# INLINE unsnoc #-}
unsnoc (Vec arr s l)
    | l <= 0    = Nothing
    | otherwise = let !v = fromArr arr s (l-1)
                  in case indexArr' arr (s+l-1) of (# x #) -> Just (v, x)

-- | /O(1)/ Extract the first element of a vector.
headMaybe :: Vec v a => v a -> Maybe a
{-# INLINE headMaybe #-}
headMaybe (Vec arr s l)
    | l <= 0    = Nothing
    | otherwise = indexArrM arr s

-- | /O(1)/ Extract the elements after the head of a vector.
--
-- NOTE: 'tailMayEmpty' return empty vector in the case of an empty vector.
tailMayEmpty :: Vec v a => v a -> v a
{-# INLINE tailMayEmpty #-}
tailMayEmpty (Vec arr s l)
    | l <= 0    = empty
    | otherwise = fromArr arr (s+1) (l-1)

-- | /O(1)/ Extract the last element of a vector.
lastMaybe :: Vec v a => v a -> Maybe a
{-# INLINE lastMaybe #-}
lastMaybe (Vec arr s l)
    | l <= 0    = Nothing
    | otherwise = indexArrM arr (s+l-1)

-- | /O(1)/ Extract the elements before of the last one.
--
-- NOTE: 'initMayEmpty' return empty vector in the case of an empty vector.
initMayEmpty :: Vec v a => v a -> v a
{-# INLINE initMayEmpty #-}
initMayEmpty (Vec arr s l)
    | l <= 0    = empty
    | otherwise = fromArr arr s (l-1)

-- | /O(n)/ Return all initial segments of the given vector, empty first.
inits :: Vec v a => v a -> [v a]
{-# INLINE inits #-}
inits (Vec arr s l) =  [Vec arr s n | n <- [0..l]]

-- | /O(n)/ Return all final segments of the given vector, whole vector first.
tails :: Vec v a => v a -> [v a]
{-# INLINE tails #-}
tails (Vec arr s l) = [Vec arr (s+n) (l-n) | n <- [0..l]]

-- | /O(1)/ 'take' @n@, applied to a vector @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Vec v a => Int -> v a -> v a
{-# INLINE take #-}
take n v@(Vec arr s l)
    | n <= 0    = empty
    | n >= l    = v
    | otherwise = fromArr arr s n

-- | /O(1)/ 'takeR' @n@, applied to a vector @xs@, returns the suffix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
takeR :: Vec v a => Int -> v a -> v a
{-# INLINE takeR #-}
takeR n v@(Vec arr s l)
    | n <= 0    = empty
    | n >= l    = v
    | otherwise = fromArr arr (s+l-n) n

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop :: Vec v a => Int -> v a -> v a
{-# INLINE drop #-}
drop n v@(Vec arr s l)
    | n <= 0    = v
    | n >= l    = empty
    | otherwise = fromArr arr (s+n) (l-n)

-- | /O(1)/ 'dropR' @n xs@ returns the prefix of @xs@ before the last @n@
-- elements, or @[]@ if @n > 'length' xs@.
dropR :: Vec v a => Int -> v a -> v a
{-# INLINE dropR #-}
dropR n v@(Vec arr s l)
    | n <= 0    = v
    | n >= l    = empty
    | otherwise = fromArr arr s (l-n)

-- | /O(1)/ Extract a sub-range vector with give start index and length.
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
slice :: Vec v a => Int     -- ^ slice beginning index
                 -> Int     -- ^ slice length
                 -> v a -> v a
{-# INLINE slice #-}
slice x y = drop x . take (x+y)

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Vec v a => Int -> v a -> (v a, v a)
{-# INLINE splitAt #-}
splitAt z (Vec arr s l) = let !v1 = fromArr arr s z'
                              !v2 = fromArr arr (s+z') (l-z')
                          in (v1, v2)
  where z' = rangeCut z 0 l

-- | /O(n)/ Applied to a predicate @p@ and a vector @vs@,
-- returns the longest prefix (possibly empty) of @vs@ of elements that
-- satisfy @p@.
takeWhile :: Vec v a => (a -> Bool) -> v a -> v a
{-# INLINE takeWhile #-}
takeWhile f v@(Vec arr s _) =
    case findIndex (not . f) v of
        0  -> empty
        i  -> Vec arr s i

-- | /O(n)/ Applied to a predicate @p@ and a vector @vs@,
-- returns the longest suffix (possibly empty) of @vs@ of elements that
-- satisfy @p@.
takeWhileR :: Vec v a => (a -> Bool) -> v a -> v a
{-# INLINE takeWhileR #-}
takeWhileR f v@(Vec arr s l) =
    case findIndexR (not . f) v of
        -1 -> v
        i  -> Vec arr (s+i+1) (l-i-1)

-- | /O(n)/ Applied to a predicate @p@ and a vector @vs@,
-- returns the suffix (possibly empty) remaining after 'takeWhile' @p vs@.
dropWhile :: Vec v a => (a -> Bool) -> v a -> v a
{-# INLINE dropWhile #-}
dropWhile f v@(Vec arr s l) =
    case findIndex (not . f) v of
        i | i == l     -> empty
          | otherwise  -> Vec arr (s+i) (l-i)

-- | /O(n)/ Applied to a predicate @p@ and a vector @vs@,
-- returns the prefix (possibly empty) remaining before 'takeWhileR' @p vs@.
dropWhileR :: Vec v a => (a -> Bool) -> v a -> v a
{-# INLINE dropWhileR #-}
dropWhileR f v@(Vec arr s _) =
    case findIndexR (not . f) v of
        -1 -> empty
        i  -> Vec arr s (i+1)

-- | /O(n)/ @dropAround f = dropWhile f . dropWhileR f@
dropAround :: Vec v a => (a -> Bool) -> v a -> v a
{-# INLINE dropAround #-}
dropAround f = dropWhile f . dropWhileR f


-- | /O(n)/ Split the vector into the longest prefix of elements that do not satisfy the predicate and the rest without copying.
--
-- @break (==x)@ will be rewritten using a @memchr@.
break :: Vec v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE break #-}
break f vs@(Vec arr s l) =
    let !n =  findIndex f vs
        !v1 = Vec arr s n
        !v2 = Vec arr (s+n) (l-n)
    in (v1, v2)

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy the predicate and the rest without copying.
--
-- @span (/=x)@ will be rewritten using a @memchr@.
span :: Vec v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE [1] span #-}
span f = break (not . f)
{-# RULES "spanNEq/breakEq1" forall w. span (w `neWord8`) = break (w `eqWord8`) #-}
{-# RULES "spanNEq/breakEq2" forall w. span (`neWord8` w) = break (`eqWord8` w) #-}

-- | 'breakR' behaves like 'break' but from the end of the vector.
--
-- @breakR p == spanR (not.p)@
breakR :: Vec v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE breakR #-}
breakR f vs@(Vec arr s l) =
    let !n = findIndexR f vs
        !v1 = Vec arr s (n+1)
        !v2 = Vec arr (s+n+1) (l-1-n)
    in (v1, v2)

-- | 'spanR' behaves like 'span' but from the end of the vector.
spanR :: Vec v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE spanR #-}
spanR f = breakR (not . f)

-- | Break a vector on a subvector, returning a pair of the part of the
-- vector prior to the match, and the rest of the vector, e.g.
--
-- > break "wor" "hello, world" = ("hello, ", "world")
--
breakOn :: (Vec v a, Eq a) => v a -> v a -> (v a, v a)
{-# INLINE breakOn #-}
breakOn needle = \ haystack@(Vec arr s l) ->
    case search haystack False of
        (i:_) -> let !v1 = Vec arr s i
                     !v2 = Vec arr (s+i) (l-i)
                 in (v1, v2)
        _     -> (haystack, empty)
  where search = indices needle


group :: (Vec v a, Eq a) => v a -> [v a]
{-# INLINE group #-}
group = groupBy (==)

groupBy :: Vec v a =>  (a -> a -> Bool) -> v a -> [v a]
{-# INLINE groupBy #-}
groupBy f (Vec arr s l)
    | l == 0    = []
    | otherwise = Vec arr s n : groupBy f (Vec arr (s+n) (l-n))
  where
    n = case indexArr' arr s of
        (# x #) -> 1 + findIndex (not . f x) (Vec arr (s+1) (l-1))

-- | /O(n)/ The 'stripPrefix' function takes two vectors and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
--
stripPrefix :: (Vec v a, Eq (v a))
            => v a      -- ^ the prefix to be tested
            -> v a -> Maybe (v a)
{-# INLINE stripPrefix #-}
stripPrefix v1@(Vec _ _ l1) v2@(Vec arr s l2)
   | v1 `isPrefixOf` v2 = Just (Vec arr (s+l1) (l2-l1))
   | otherwise = Nothing

-- | The 'isPrefix' function returns 'True' if the first argument is a prefix of the second.
isPrefixOf :: (Vec v a, Eq (v a))
           => v a       -- ^ the prefix to be tested
           -> v a -> Bool
{-# INLINE isPrefixOf #-}
isPrefixOf (Vec arrA sA lA) (Vec arrB sB lB)
    | lA == 0 = True
    | lA > lB = False
    | otherwise = Vec arrA sA lA == Vec arrB sB lA

-- | /O(n)/ Find the longest non-empty common prefix of two strings
-- and return it, along with the suffixes of each string at which they
-- no longer match. e.g.
--
-- >>> commonPrefix "foobar" "fooquux"
-- ("foo","bar","quux")
--
-- >>> commonPrefix "veeble" "fetzer"
-- ("","veeble","fetzer")
commonPrefix :: (Vec v a, Eq a) => v a -> v a -> (v a, v a, v a)
{-# INLINE commonPrefix #-}
commonPrefix vA@(Vec arrA sA lA) vB@(Vec arrB sB lB) = go sA sB
  where
    !endA = sA + lA
    !endB = sB + lB
    go !i !j | i >= endA = let !vB' = fromArr arrB (sB+i-sA) (lB-i+sA) in (vA, empty, vB')
             | j >= endB = let !vA' = fromArr arrA (sA+j-sB) (lA-j+sB) in (vB, vA', empty)
             | indexArr arrA i == indexArr arrB j = go (i+1) (j+1)
             | otherwise =
                let !vB' = fromArr arrB (sB+i-sA) (lB-i+sA)
                    !vA' = fromArr arrA (sA+j-sB) (lA-j+sB)
                    !vC  = fromArr arrA sA (i-sA)
                in (vC, vA', vB')

-- | O(n) The 'stripSuffix' function takes two vectors and returns Just the remainder of the second iff the first is its suffix, and otherwise Nothing.
stripSuffix :: (Vec v a, Eq (v a)) => v a -> v a -> Maybe (v a)
{-# INLINE stripSuffix #-}
stripSuffix v1@(Vec _ _ l1) v2@(Vec arr s l2)
   | v1 `isSuffixOf` v2 = Just (Vec arr s (l2-l1))
   | otherwise = Nothing

-- | /O(n)/ The 'isSuffixOf' function takes two vectors and returns 'True'
-- if the first is a suffix of the second.
isSuffixOf :: (Vec v a, Eq (v a)) => v a -> v a -> Bool
{-# INLINE isSuffixOf #-}
isSuffixOf (Vec arrA sA lA) (Vec arrB sB lB)
    | lA == 0 = True
    | lA > lB = False
    | otherwise = Vec arrA sA lA == Vec arrB (sB+lB-lA) lA

-- | Check whether one vector is a subvector of another.
--
-- @needle `isInfixOf` haystack === null haystack || indices needle haystake /= []@.
isInfixOf :: (Vec v a, Eq a) => v a -> v a -> Bool
{-# INLINE isInfixOf #-}
isInfixOf needle = \ haystack -> null haystack || search haystack False /= []
  where search = indices needle

-- | /O(n)/ Break a vector into pieces separated by the delimiter element
-- consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X",""]
-- > split 'x'  "x"          == ["",""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- NOTE, this function behavior different with bytestring's. see
-- <https://github.com/haskell/bytestring/issues/56 #56>.
split :: (Vec v a, Eq a) => a -> v a -> [v a]
{-# INLINE split #-}
split x = splitWith (==x)

-- | /O(m+n)/ Break haystack into pieces separated by needle.
--
-- Note: An empty needle will essentially split haystack element
-- by element.
--
-- Examples:
--
-- >>> splitOn "\r\n" "a\r\nb\r\nd\r\ne"
-- ["a","b","d","e"]
--
-- >>> splitOn "aaa"  "aaaXaaaXaaaXaaa"
-- ["","X","X","X",""]
--
-- >>> splitOn "x"  "x"
-- ["",""]
--
-- and
--
-- > intercalate s . splitOn s         == id
-- > splitOn (singleton c)             == split (==c)
splitOn :: (Vec v a, Eq a) => v a -> v a -> [v a]
{-# INLINE splitOn #-}
splitOn needle = splitBySearch
  where
    splitBySearch haystack@(Vec arr s l) = go s (search haystack False)
      where
        !l' = length needle
        !end = s+l
        search = indices needle
        go !s' (i:is) = let !v = fromArr arr s' (i+s-s')
                               in v : go (i+l') is
        go !s' _      = let !v = fromArr arr s' (end-s') in [v]

-- | /O(n)/ Splits a vector into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith (=='a') []        == [""]
--
-- NOTE, this function behavior different with bytestring's. see
-- <https://github.com/haskell/bytestring/issues/56 #56>.
splitWith :: Vec v a => (a -> Bool) -> v a -> [v a]
{-# INLINE splitWith #-}
splitWith f (Vec arr s l) = go s s
  where
    !end = s + l
    go !p !q | q >= end  = let !v = Vec arr p (q-p) in [v]
             | f x       = let !v = Vec arr p (q-p) in v:go (q+1) (q+1)
             | otherwise = go p (q+1)
        where (# x #) = indexArr' arr q

-- | /O(n)/ Breaks a 'Bytes' up into a list of words, delimited by ascii space.
words ::  Bytes -> [Bytes]
{-# INLINE words #-}
words (Vec arr s l) = go s s
  where
    !end = s + l
    go !s' !i | i >= end =
                    if s' == end
                    then []
                    else let !v = fromArr arr s' (end-s') in [v]
              | isASCIISpace (indexArr arr i) =
                    if s' == i
                    then go (i+1) (i+1)
                    else
                    let !v = fromArr arr s' (i-s') in v : go (i+1) (i+1)
              | otherwise = go s' (i+1)

-- | /O(n)/ Breaks a 'Bytes' up into a list of lines, delimited by ascii @\n@.
lines ::  Bytes -> [Bytes]
{-# INLINE lines #-}
lines (Vec arr s l) = go s s
  where
    !end = s + l
    go !p !q | q >= end              = if p == q
                                       then []
                                       else let !v = Vec arr p (q-p) in [v]
             | indexArr arr q == 10  = let !v = Vec arr p (q-p) in v:go (q+1) (q+1)
             | otherwise             = go p (q+1)

-- | /O(n)/ Joins words with ascii space.
unwords :: [Bytes] -> Bytes
{-# INLINE unwords #-}
unwords = intercalateElem 32

-- | /O(n)/ Joins lines with ascii @\n@.
unlines :: [Bytes] -> Bytes
{-# INLINE unlines #-}
unlines = intercalateElem 10

-- | Add padding to the left so that the whole vector's length is at least n.
padLeft :: Vec v a => Int -> a -> v a -> v a
{-# INLINE padLeft #-}
padLeft n x v@(Vec arr s l) | n <= l = v
                            | otherwise = create n (\ marr -> do
                                    setArr marr 0 (n-l) x
                                    copyArr marr (n-l) arr s l)

-- | Add padding to the right so that the whole vector's length is at least n.
padRight :: Vec v a => Int -> a -> v a -> v a
{-# INLINE padRight #-}
padRight n x v@(Vec arr s l) | n <= l = v
                             | otherwise = create n (\ marr -> do
                                    copyArr marr 0 arr s l
                                    setArr marr l (n-l) x)

--------------------------------------------------------------------------------
-- Transform

-- | /O(n)/ 'reverse' @vs@ efficiently returns the elements of @xs@ in reverse order.
--
reverse :: forall v a. (Vec v a) => v a -> v a
{-# INLINE reverse #-}
reverse (Vec arr s l) = create l (go s (l-1))
  where
    go :: Int -> Int -> MArray v s a -> ST s ()
    go !i !j !marr | j < 0 = return ()
                   | j >= 3 = do  -- a bit of loop unrolling here
                        indexArrM arr i >>= writeArr marr j
                        indexArrM arr (i+1) >>= writeArr marr (j-1)
                        indexArrM arr (i+2) >>= writeArr marr (j-2)
                        indexArrM arr (i+3) >>= writeArr marr (j-3)
                        go (i+4) (j-4) marr
                   | otherwise = do
                        indexArrM arr i >>= writeArr marr j
                        go (i+1) (j-1) marr

-- | /O(n)/ The 'intersperse' function takes an element and a
-- vector and \`intersperses\' that element between the elements of
-- the vector.  It is analogous to the intersperse function on
-- Lists.
--
intersperse :: forall v a. Vec v a => a -> v a -> v a
{-# INLINE intersperse #-}
intersperse x v@(Vec arr s l) | l <= 1 = v
                              | otherwise = create (2*l-1) (go s 0)
   where
    !end = s+l-1
    go :: Int         -- the reading index of orginal bytes
       -> Int         -- the writing index of new buf
       -> MArray v s a -- the new buf
       -> ST s ()
    go !i !j !marr
        | i >= end = writeArr marr j =<< indexArrM arr i
        | i <= end - 4 = do -- a bit of loop unrolling
            writeArr marr j =<< indexArrM arr i
            writeArr marr (j+1) x
            writeArr marr (j+2) =<< indexArrM arr (i+1)
            writeArr marr (j+3) x
            writeArr marr (j+4) =<< indexArrM arr (i+2)
            writeArr marr (j+5) x
            writeArr marr (j+6) =<< indexArrM arr (i+3)
            writeArr marr (j+7) x
            go (i+4) (j+8) marr
        | otherwise = do
            writeArr marr j =<< indexArrM arr i
            writeArr marr (j+1) x
            go (i+1) (j+2) marr

-- | /O(n)/ The 'intercalate' function takes a vector and a list of
-- vectors and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- Note: 'intercalate' will force the entire vector list.
--
intercalate :: Vec v a => v a -> [v a] -> v a
{-# INLINE intercalate #-}
intercalate s = concat . List.intersperse s

-- | /O(n)/ An efficient way to join vector with an element.
--
intercalateElem :: Vec v a => a -> [v a] -> v a
{-# INLINE intercalateElem #-}
intercalateElem _ [] = empty
intercalateElem _ [v] = v
intercalateElem w vs = create (len vs 0) (go 0 vs)
  where
    len []             !acc = acc
    len [Vec _ _ l]    !acc = l + acc
    len (Vec _ _ l:vs') !acc = len vs' (acc+l+1)
    go !_ []               !_    = return ()
    go !i (Vec arr s l:[]) !marr = copyArr marr i arr s l
    go !i (Vec arr s l:vs') !marr = do
        let !i' = i + l
        copyArr marr i arr s l
        writeArr marr i' w
        go (i'+1) vs' marr

-- | The 'transpose' function transposes the rows and columns of its
-- vector argument.
--
transpose :: Vec v a => [v a] -> [v a]
{-# INLINE transpose #-}
transpose vs =
    List.map (packN n) . List.transpose . List.map unpack $ vs
  where n = List.length vs

--------------------------------------------------------------------------------
--  Zipping

-- | 'zipWith'' zip two vector with a zipping function.
--
-- For example, @'zipWith' (+)@ is applied to two vector to produce
-- a vector of corresponding sums, the result will be evaluated strictly.
zipWith' :: (Vec v a, Vec u b, Vec w c)
         => (a -> b -> c) -> v a -> u b -> w c
{-# INLINE zipWith' #-}
zipWith' f (Vec arrA sA lA) (Vec arrB sB lB) = create len (go 0)
  where
    !len = min lA lB
    go !i !marr
        | i >= len = return ()
        | otherwise = case indexArr' arrA (i+sA) of
             (# a #) -> case indexArr' arrB (i+sB) of
                 (# b #) -> do let !c = f a b in writeArr marr i c
                               go (i+1) marr

-- | 'unzipWith'' disassemble a vector with a disassembling function,
--
-- The results inside tuple will be evaluated strictly.
unzipWith' :: (Vec v a, Vec u b, Vec w c)
           => (a -> (b, c)) -> v a -> (u b, w c)
{-# INLINE unzipWith' #-}
unzipWith' f (Vec arr s l) = createN2 l l (go 0)
  where
    go !i !marrB !marrC
        | i >= l = return (l,l)
        | otherwise = case indexArr' arr (i+s) of
            (# a #) -> do let (!b, !c) = f a
                          writeArr marrB i b
                          writeArr marrC i c
                          go (i+1) marrB marrC

--------------------------------------------------------------------------------
-- Scans

-- | 'scanl'' is similar to 'foldl', but returns a list of successive
-- reduced values from the left.
--
-- > scanl' f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > lastM (scanl' f z xs) == Just (foldl f z xs).
--
scanl' :: forall v u a b. (Vec v a, Vec u b) => (b -> a -> b) -> b -> v a -> u b
{-# INLINE scanl' #-}
scanl' f z (Vec arr s l) =
    create (l+1) (\ marr -> writeArr marr 0 z >> go z s 1 marr)
  where
    go :: b  -> Int -> Int -> MArray u s b -> ST s ()
    go !acc !i !j !marr
        | j > l = return ()
        | otherwise = do
            x <- indexArrM arr i
            let !acc' = acc `f` x
            writeArr marr j acc'
            go acc' (i+1) (j+1) marr

-- | 'scanl1\'' is a variant of 'scanl' that has no starting value argument.
--
-- > scanl1' f [x1, x2, ...] == [x1, x1 `f` x2, ...]
-- > scanl1' f [] == []
--
scanl1' :: forall v a. Vec v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanl1' #-}
scanl1' f (Vec arr s l)
    | l <= 0    = empty
    | otherwise = case indexArr' arr s of
                    (# x0 #) -> scanl' f x0 (fromArr arr (s+1) (l-1) :: v a)

-- | scanr' is the right-to-left dual of scanl'.
--
scanr' :: forall v u a b. (Vec v a, Vec u b) => (a -> b -> b) -> b -> v a -> u b
{-# INLINE scanr' #-}
scanr' f z (Vec arr s l) =
    create (l+1) (\ marr -> writeArr marr l z >> go z (s+l-1) (l-1) marr)
  where
    go :: b -> Int -> Int -> MArray u s b -> ST s ()
    go !acc !i !j !marr
        | j < 0 = return ()
        | otherwise = do
            x <- indexArrM arr i
            let !acc' = x `f` acc
            writeArr marr j acc'
            go acc' (i-1) (j-1) marr

-- | 'scanr1'' is a variant of 'scanr' that has no starting value argument.
scanr1' :: forall v a. Vec v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanr1' #-}
scanr1' f (Vec arr s l)
    | l <= 0    = empty
    | otherwise = case indexArr' arr (s+l-1) of
                    (# x0 #) -> scanr' f x0 (fromArr arr s (l-1) :: v a)

--------------------------------------------------------------------------------
-- Misc

-- | @x' = rangeCut x min max@ limit @x'@ 's range to @min@ ~ @max@.
rangeCut :: Int -> Int -> Int -> Int
{-# INLINE rangeCut #-}
rangeCut !r !min' !max' | r < min' = min'
                        | r > max' = max'
                        | otherwise = r

isASCIISpace :: Word8 -> Bool
{-# INLINE isASCIISpace #-}
isASCIISpace w = w == 32 || w - 0x9 <= 4 || w == 0xa0

--------------------------------------------------------------------------------

-- | /O(1)/ Extract the first element of a vector.
--
-- Throw 'EmptyVector' if vector is empty.
head :: (Vec v a, HasCallStack) => v a -> a
{-# INLINE head #-}
head (Vec arr s l)
    | l <= 0    = errorEmptyVector
    | otherwise = indexArr arr s

-- | /O(1)/ Extract the elements after the head of a vector.
--
-- Throw 'EmptyVector' if vector is empty.
tail :: (Vec v a, HasCallStack) => v a -> v a
{-# INLINE tail #-}
tail (Vec arr s l)
    | l <= 0    = errorEmptyVector
    | otherwise = fromArr arr (s+1) (l-1)

-- | /O(1)/ Extract the elements before of the last one.
--
-- Throw 'EmptyVector' if vector is empty.
init :: (Vec v a, HasCallStack) => v a -> v a
{-# INLINE init #-}
init (Vec arr s l)
    | l <= 0    = errorEmptyVector
    | otherwise = fromArr arr s (l-1)

-- | /O(1)/ Extract the last element of a vector.
--
-- Throw 'EmptyVector' if vector is empty.
last :: (Vec v a, HasCallStack) => v a -> a
{-# INLINE last #-}
last (Vec arr s l)
    | l <= 0    = errorEmptyVector
    | otherwise = indexArr arr (s+l-1)

-- | /O(1)/ Index array element.
--
-- Throw 'IndexOutOfVectorRange' if index outside of the vector.
index :: (Vec v a, HasCallStack) => v a -> Int -> a
{-# INLINE index #-}
index (Vec arr s l) i | i < 0 || i >= l = errorOutRange i
                      | otherwise       = arr `indexArr` (s + i)

-- | /O(1)/ Index array element.
--
-- Throw 'IndexOutOfVectorRange' if index outside of the vector.
indexM :: (Vec v a, Monad m, HasCallStack) => v a -> Int -> m a
{-# INLINE indexM #-}
indexM (Vec arr s l) i | i < 0 || i >= l = errorOutRange i
                       | otherwise       = arr `indexArrM` (s + i)

-- | /O(1)/ Extract the first element of a vector.
--
-- Make sure vector is non-empty, otherwise segmentation fault await!
unsafeHead  :: Vec v a => v a -> a
{-# INLINE unsafeHead #-}
unsafeHead (Vec arr s l) = assert (l > 0) (indexArr arr s)

-- | /O(1)/ Extract the elements after the head of a vector.
--
-- Make sure vector is non-empty, otherwise segmentation fault await!
unsafeTail  :: Vec v a => v a -> v a
{-# INLINE unsafeTail #-}
unsafeTail (Vec arr s l) = assert (l > 0) (fromArr arr (s+1) (l-1))

-- | /O(1)/ Extract the elements before of the last one.
--
-- Make sure vector is non-empty, otherwise segmentation fault await!
unsafeInit  :: Vec v a => v a -> v a
{-# INLINE unsafeInit #-}
unsafeInit (Vec arr s l) = assert (l > 0) (fromArr arr s (l-1))

-- | /O(1)/ Extract the last element of a vector.
--
-- Make sure vector is non-empty, otherwise segmentation fault await!
unsafeLast  :: Vec v a => v a -> a
{-# INLINE unsafeLast #-}
unsafeLast (Vec arr s l) = assert (l > 0) (indexArr arr (s+l-1))

-- | /O(1)/ Index array element.
--
-- Make sure index is in bound, otherwise segmentation fault await!
unsafeIndex :: Vec v a => v a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex (Vec arr s _) i = indexArr arr (s + i)

-- | /O(1)/ Index array element.
--
-- Make sure index is in bound, otherwise segmentation fault await!
unsafeIndexM :: (Vec v a, Monad m) => v a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM (Vec arr s _) i = indexArrM arr (s + i)

-- | /O(1)/ 'take' @n@, applied to a vector @xs@, returns the prefix
-- of @xs@ of length @n@.
--
-- Make sure n is smaller than vector's length, otherwise segmentation fault await!
unsafeTake :: Vec v a => Int -> v a -> v a
{-# INLINE unsafeTake #-}
unsafeTake n (Vec arr s l) = assert (0 <= n && n <= l) (fromArr arr s n)

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements.
--
-- Make sure n is smaller than vector's length, otherwise segmentation fault await!
unsafeDrop :: Vec v a => Int -> v a -> v a
{-# INLINE unsafeDrop #-}
unsafeDrop n (Vec arr s l) = assert (0 <= n && n <= l) (fromArr arr (s+n) (l-n))
