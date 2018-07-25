{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Std.Data.Vector.Extra (
  -- * Slice manipulation
    cons, snoc
  , uncons, unsnoc
  , headM, tailM
  , lastM, initM
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
  -- * Search
  -- ** element-wise search
  , findIndices, elemIndices, elemIndicesBytes
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
  -- * QuasiQuoters
  , ascii
  , vecW8, vecW16, vecW32, vecW64, vecWord
  , vecI8, vecI16, vecI32, vecI64, vecInt
  -- * Misc
  , rangeCut
  ) where

import           Control.Monad.ST
import           GHC.Stack
import           GHC.Word
import           Std.Data.Array
import           Std.Data.Vector.Base
import qualified Language.Haskell.TH.Quote     as QQ
import           Std.Data.PrimArray.QQ         as QQ
import           Prelude                       hiding (concat, concatMap,
                                                elem, notElem, null, length, map,
                                                foldl, foldl1, foldr, foldr1,
                                                maximum, minimum, product, sum,
                                                all, any, replicate, traverse,
                                                take, drop, splitAt,
                                                takeWhile, dropWhile,
                                                break, span, reverse, filter)
import qualified Data.List                     as List
import           Std.Data.PrimArray.BitTwiddle (c_memchr, memchrReverse)
import           Data.Bits
import           Debug.Trace

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
-- NOTE: 'tailM' return empty vector in the case of an empty vector.
tailM :: Vec v a => v a -> v a
{-# INLINE tailM #-}
tailM (Vec arr s l)
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
-- NOTE: 'initM' return empty vector in the case of an empty vector.
initM :: Vec v a => v a -> v a
{-# INLINE initM #-}
initM (Vec arr s l)
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
    s'' = rangeCut (s+s') s (s+l)
    l'' = rangeCut l' 0 (s+l-s'')

-- | /O(1)/ Extract a sub-range vector with give start and end index, both
-- start and end index can be negative, which stand for counting from the end
-- (similar to the slicing operator([..]) in many other language).
--
-- This function is a total function just like 'take/drop', e.g.
--
-- @
-- "hello" |..| (1, 3) == "el"
-- "hello" |..| (1, -1) == "ell"
-- slice "hello" (-3, -2) == "l"
-- slice "hello" (-3, -4) == ""
-- @
--
(|..|) :: Vec v a => v a -> (Int, Int) -> v a
{-# INLINE (|..|) #-}
(Vec arr s l) |..| (s1, s2) | s1' <= s2' = empty
                               | otherwise  = fromArr arr s1' (s2' - s1')
  where
    s1' = rangeCut (if s1>0 then s+s1 else s+l+s1) s (s+l)
    s2' = rangeCut (if s2>0 then s+s2 else s+l+s2) s (s+l)

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Vec v a => Int -> v a -> (v a, v a)
{-# INLINE splitAt #-}
splitAt z (Vec arr s l) = let !v1 = fromArr arr s z'
                              !v2 = fromArr arr (s+z') (l-z')
                          in (v1, v2)
  where z' = rangeCut 0 z l

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
            case fromIntegral (c_memchr ba# i w (end - i)) of
                -1 -> []
                r  -> r : go (i + r)

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
    go !p | p >= end  = end
          | f x       = p-s
          | otherwise = go (p+1)
        where (# x #) = indexArr' arr p

-- | /O(n)/ Special 'elemIndexOrEndBytes' for 'Bytes' using @memchr(3)@
elemIndexOrEndBytes :: Word8 -> Bytes -> Int
{-# INLINE elemIndexOrEndBytes #-}
elemIndexOrEndBytes w (PrimVector (PrimArray ba#) s l) =
    case fromIntegral (c_memchr ba# s w l) of
        -1 -> l
        r  -> r

-- | /O(n)/ find the first index matching the predicate in a vector
-- from right to left, if there isn't one, return zero.
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
findIndexReverseOrStart f (Vec arr s l) = go s
  where
    !end = s + l
    go !p | p >= end  = end
          | f x       = p-s
          | otherwise = go (p+1)
        where (# x #) = indexArr' arr p

-- | /O(n)/ Special 'elemIndexReverseOrStartBytes' for 'Bytes' with handle roll
-- bit twiddling.
elemIndexReverseOrStartBytes :: Word8 -> Bytes -> Int
{-# INLINE elemIndexReverseOrStartBytes #-}
elemIndexReverseOrStartBytes w (PrimVector ba s l) =
    case memchrReverse ba w (s+l-1) l of
        -1 -> 0
        r  -> r

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
-- the worst case time complexity is /O(n+m)/. Partial applied this function to
-- reuse pre-calculated table between same needles.
--
-- Chunked input are support via partial match argument, if set we will return an
-- extra negative index in case of partial match at the end of input chunk, e.g.
--
-- > indicesOverlapping [ascii|ada|]  [ascii|adadad|] True == [0,2,-2]
--
-- Where @-2@ is the length of the partial match part @ad@.
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

--------------------------------------------------------------------------------
-- Quoters

ascii :: QQ.QuasiQuoter
ascii = QQ.QuasiQuoter
    (asciiLiteral $ \ l addr -> [| PrimVector (QQ.word8ArrayFromAddr l $(addr)) 0 l |])
    (error "Cannot use ascii as a pattern")
    (error "Cannot use ascii as a type")
    (error "Cannot use ascii as a dec")

vecW8 :: QQ.QuasiQuoter
vecW8 = QQ.QuasiQuoter
    (QQ.word8Literal $ \ len addr -> [| PrimVector (QQ.word8ArrayFromAddr len $(addr)) 0 len |])
    (error "Cannot use vecW8 as a pattern")
    (error "Cannot use vecW8 as a type")
    (error "Cannot use vecW8 as a dec")

vecW16 :: QQ.QuasiQuoter
vecW16 = QQ.QuasiQuoter
    (QQ.word16Literal $ \ len addr -> [| PrimVector (QQ.word16ArrayFromAddr len $(addr)) 0 len |])
    (error "Cannot use vecW16 as a pattern")
    (error "Cannot use vecW16 as a type")
    (error "Cannot use vecW16 as a dec")

vecW32 :: QQ.QuasiQuoter
vecW32 = QQ.QuasiQuoter
    (QQ.word32Literal $ \ len addr -> [| PrimVector (QQ.word32ArrayFromAddr len $(addr)) 0 len |])
    (error "Cannot use vecW32 as a pattern")
    (error "Cannot use vecW32 as a type")
    (error "Cannot use vecW32 as a dec")

vecW64 :: QQ.QuasiQuoter
vecW64 = QQ.QuasiQuoter
    (QQ.word64Literal $ \ len addr -> [| PrimVector (QQ.word64ArrayFromAddr len $(addr)) 0 len |])
    (error "Cannot use vecW64 as a pattern")
    (error "Cannot use vecW64 as a type")
    (error "Cannot use vecW64 as a dec")

vecWord :: QQ.QuasiQuoter
vecWord = QQ.QuasiQuoter
    (QQ.wordLiteral $ \ len addr ->
        [| PrimVector (QQ.wordArrayFromAddr len $(addr)) 0 len |])
    (error "Cannot use vecWord as a pattern")
    (error "Cannot use vecWord as a type")
    (error "Cannot use vecWord as a dec")

vecI8 :: QQ.QuasiQuoter
vecI8 = QQ.QuasiQuoter
    (QQ.int8Literal $ \ len addr ->
        [| PrimVector (QQ.word8ArrayFromAddr len $(addr)) 0 len |])
    (error "Cannot use vecI8 as a pattern")
    (error "Cannot use vecI8 as a type")
    (error "Cannot use vecI8 as a dec")

vecI16 :: QQ.QuasiQuoter
vecI16 = QQ.QuasiQuoter
    (QQ.int16Literal $ \ len addr ->
        [| PrimVector (QQ.int16ArrayFromAddr len $(addr)) 0 len |])
    (error "Cannot use vecI16 as a pattern")
    (error "Cannot use vecI16 as a type")
    (error "Cannot use vecI16 as a dec")

vecI32 :: QQ.QuasiQuoter
vecI32 = QQ.QuasiQuoter
    (QQ.int32Literal $ \ len addr ->
        [| PrimVector (QQ.int32ArrayFromAddr len $(addr)) 0 len |])
    (error "Cannot use vecI32 as a pattern")
    (error "Cannot use vecI32 as a type")
    (error "Cannot use vecI32 as a dec")

vecI64 :: QQ.QuasiQuoter
vecI64 = QQ.QuasiQuoter
    (QQ.int64Literal $ \ len addr ->
        [| PrimVector (QQ.int64ArrayFromAddr len $(addr)) 0 len |])
    (error "Cannot use vecI64 as a pattern")
    (error "Cannot use vecI64 as a type")
    (error "Cannot use vecI64 as a dec")

vecInt :: QQ.QuasiQuoter
vecInt = QQ.QuasiQuoter
    (QQ.intLiteral $ \ len addr ->
        [| PrimVector (QQ.intArrayFromAddr len $(addr)) 0 len |])
    (error "Cannot use vecInt as a pattern")
    (error "Cannot use vecInt as a type")
    (error "Cannot use vecInt as a dec")

--------------------------------------------------------------------------------
-- Misc

-- | @x' = rangeCut x min max@ limit @x'@ 's range to @min@ ~ @max@.
rangeCut :: Int -> Int -> Int -> Int
{-# INLINE rangeCut #-}
rangeCut !r !min !max | r < min = min
                      | r > max = max
                      | otherwise = r

