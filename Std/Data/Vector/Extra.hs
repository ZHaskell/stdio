{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
Module      : Std.Data.Vector.Extra
Description : Fast boxed and unboxed vector
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

Various combinators works on 'Vec' class instances.

-}

module Std.Data.Vector.Extra (
  -- * Slice manipulation
    cons, snoc
  , uncons, unsnoc
  , headM, tailE
  , lastM, initE
  , inits, tails
  , take, drop
  , slice , (|..|)
  , splitAt
  , takeWhile , dropWhile
  , break, span
  , breakEnd, spanEnd
  , group, groupBy
  , stripPrefix, stripSuffix
  , split, splitWith
  , isPrefixOf, isSuffixOf, isInfixOf
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
                                                take, drop, splitAt,
                                                takeWhile, dropWhile,
                                                break, span, reverse)
import qualified Data.List                     as List
import           Data.Bits

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

-- | /O(1)/ Extract the first element of a vector,
headM :: Vec v a => v a -> Maybe a
{-# INLINE headM #-}
headM (Vec arr s l)
    | l <= 0    = Nothing
    | otherwise = indexArrM arr s

-- | /O(1)/ Extract the elements after the head of a vector.
--
-- NOTE: 'tailE' return empty vector in the case of an empty vector.
tailE :: Vec v a => v a -> v a
{-# INLINE tailE #-}
tailE (Vec arr s l)
    | l <= 0    = empty
    | otherwise = fromArr arr (s+1) (l-1)

-- | /O(1)/ Extract the last element of a vector.
lastM :: Vec v a => v a -> Maybe a
{-# INLINE lastM #-}
lastM (Vec arr s l)
    | l <= 0    = Nothing
    | otherwise = indexArrM arr (s+l-1)

-- | /O(1)/ Extract the elements before of the last one,
--
-- NOTE: 'initE' return empty vector in the case of an empty vector.
initE :: Vec v a => v a -> v a
{-# INLINE initE #-}
initE (Vec arr s l)
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

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop :: Vec v a => Int -> v a -> v a
{-# INLINE drop #-}
drop n v@(Vec arr s l)
    | n <= 0    = v
    | n >= l    = empty
    | otherwise = fromArr arr (s+n) (l-n)

-- | /O(1)/ Extract a sub-range vector with give start index and length.
--
-- This function is a total function just like 'take/drop', index/length
-- exceeds range will be ingored, e.g.
--
-- @
-- slice 1 3 "hello"   == "ell"
-- slice -1 -1 "hello" == ""
-- slice 2 10 "hello"  == "llo"
-- @
--
slice :: Vec v a => Int -> Int -> v a -> v a
{-# INLINE slice #-}
slice s' l' (Vec arr s l) | l'' == 0  = empty
                          | otherwise = fromArr arr s'' l''
  where
    !s'' = rangeCut (s+s') s (s+l)
    !l'' = rangeCut l' 0 (s+l-s'')

-- | /O(1)/ Extract a sub-range vector with give start and end index
-- (sliced vector will include the end index element), both
-- start and end index can be negative, which stand for counting from the end
-- (similar to the slicing operator([..]) in many other language).
--
-- This function is a total function just like 'take/drop', e.g.
--
-- @
-- "hello" |..| (1, 2) == "el"
-- "hello" |..| (1, -2) == "ell"
-- slice "hello" (-3, -2) == "ll"
-- slice "hello" (-3, -4) == ""
-- @
infixl 9 |..|
(|..|) :: Vec v a => v a -> (Int, Int) -> v a
{-# INLINE (|..|) #-}
(Vec arr s l) |..| (s1, s2) | s1' >  s2' = empty
                            | otherwise  = fromArr arr s1' (s2' - s1'+1)
  where
    !s1' = rangeCut (if s1>=0 then s+s1 else s+l+s1) s (s+l)
    !s2' = rangeCut (if s2>=0 then s+s2 else s+l+s2) s (s+l)

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
takeWhile f v@(Vec arr s l) =
    case findIndexOrEnd (not . f) v of
        0  -> empty
        i  -> Vec arr s i

-- | /O(n)/ Applied to a predicate @p@ and a vector @vs@,
-- returns the suffix (possibly empty) remaining after 'takeWhile' @p vs@.
dropWhile :: Vec v a => (a -> Bool) -> v a -> v a
{-# INLINE dropWhile #-}
dropWhile f v@(Vec arr s l) =
    case findIndexOrEnd (not . f) v of
        i | i == l     -> empty
          | otherwise  -> Vec arr (s+i) (l-i)

break :: Vec v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE break #-}
break f vs@(Vec arr s l) = case findIndexOrEnd f vs of
    n -> let !v1 = Vec arr s n
             !v2 = Vec arr (s+n) (l-n)
         in (v1, v2)

span :: Vec v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE span #-}
span f = break (not . f)

breakEnd :: Vec v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE breakEnd #-}
breakEnd f vs@(Vec arr s l) = case findIndexReverseOrStart f vs of
    n -> let !v1 = Vec arr s n
             !v2 = Vec arr (s+n) (l-n)
         in (v1, v2)

spanEnd :: Vec v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE spanEnd #-}
spanEnd f = breakEnd (not . f)

group :: (Vec v a, Eq a) => v a -> [v a]
{-# INLINE group #-}
group = groupBy (==)

groupBy :: Vec v a =>  (a -> a -> Bool) -> v a -> [v a]
{-# INLINE groupBy #-}
groupBy f vs@(Vec arr s l)
    | l == 0    = []
    | otherwise = (Vec arr s n) : groupBy f (Vec arr (s+n) (l-n))
  where
    n = case indexArr' arr s of
        (# x #) -> 1 + findIndexOrEnd (not . f x) (Vec arr (s+1) l)

-- | /O(n)/ The 'stripPrefix' function takes two vectors and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
--
stripPrefix :: (Vec v a, Eq (v a)) => v a -> v a -> Maybe (v a)
{-# INLINE stripPrefix #-}
stripPrefix v1@(Vec _ _ l1) v2@(Vec arr s l2)
   | v1 `isPrefixOf` v2 = Just (Vec arr (s+l1) (l2-l1))
   | otherwise = Nothing

isPrefixOf :: (Vec v a, Eq (v a)) => v a -> v a -> Bool
{-# INLINE isPrefixOf #-}
isPrefixOf (Vec arrA sA lA) (Vec arrB sB lB)
    | lA == 0 = True
    | lA > lB = False
    | otherwise = Vec arrA sA lA == Vec arrB sB lA

stripSuffix :: (Vec v a, Eq (v a)) => v a -> v a -> Maybe (v a)
{-# INLINE stripSuffix #-}
stripSuffix v1@(Vec _ _ l1) v2@(Vec arr s l2)
   | v1 `isSuffixOf` v2 = Just (Vec arr s (l2-l1))
   | otherwise = Nothing

-- | /O(n)/ The 'isSuffixOf' function takes two vectors and returns 'True'
-- iff the first is a suffix of the second.
isSuffixOf :: (Vec v a, Eq (v a)) => v a -> v a -> Bool
{-# INLINE isSuffixOf #-}
isSuffixOf (Vec arrA sA lA) (Vec arrB sB lB)
    | lA == 0 = True
    | lA > lB = False
    | otherwise = Vec arrA sA lA == Vec arrB (sB+lB-lA) lA

-- | Check whether one vector is a subvector of another.
--
-- @needle `isInfixOf` haystack@ is equivalent to @indices needle haystake /= []@.
isInfixOf :: (Vec v a, Eq a) => v a -> v a -> Bool
isInfixOf needle haystack = indices needle haystack False /= []

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
split :: (Vec v a, Eq a) => a -> v a -> [v a]
{-# INLINE split #-}
split x = splitWith (==x)

-- | /O(n)/ Splits a vector into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith (=='a') []        == []
splitWith :: Vec v a => (a -> Bool) -> v a -> [v a]
{-# INLINE splitWith #-}
splitWith f (Vec arr s l) = go s s
  where
    !end = s + l
    go !p !q | q >= end  = let v = Vec arr p (q-p) in [v]
             | f x       = let v = Vec arr p (q-p) in v:go q (q+1)
             | otherwise = go p (q+1)
        where (# x #) = indexArr' arr q

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
intersperse x v@(Vec _ _ 0)  = empty
intersperse x v@(Vec _ _ 1)  = v
intersperse x v@(Vec arr s l) = create (2*l-1) (go s 0)
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
intercalateElem w vs = create (len vs) (copy 0 vs)
  where
    len []             = 0
    len [Vec _ _ l]    = l
    len (Vec _ _ l:vs) = l + 1 + len vs
    copy !i []                 !marr = return ()
    copy !i (Vec arr s l:vs) !marr = do
        let !i' = i + l
        copyArr marr i arr s l
        copy i' vs marr

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
rangeCut !r !min !max | r < min = min
                      | r > max = max
                      | otherwise = r
