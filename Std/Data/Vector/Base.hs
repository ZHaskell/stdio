{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples          #-}
{-# LANGUAGE ViewPatterns           #-}

{-|
Module      : Std.Data.Vector.Base
Description : Fast boxed and unboxed vector
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide unified vector interface. Conceptually a vector is simply a slice of an array, for example this is the definition of boxed vector:

@
    data Vector a = Vector
        {-# UNPACK #-} !(SmallArray a) -- payload
        {-# UNPACK #-} !Int         -- offset
        {-# UNPACK #-} !Int         -- length
@

The 'Vec' class unified different type of vectors, and this module provide both boxed 'Vector' and unboxed 'PrimVector', with basic operations and various useful instances.

-}

module Std.Data.Vector.Base (
  -- * The Vec typeclass
    Vec(..)
  , pattern Vec
  -- * Boxed and unboxed vector type
  , Vector(..)
  , PrimVector(..)
  -- ** Word8 vector
  , Bytes
  , pattern Bytes#
  , w2c, c2w
  -- * Basic creating
  , create, creating, createN, createN2
  , empty, singleton, copy
  -- * Conversion between list
  , pack, packN, packR, packRN
  , unpack, unpackR
  -- * Basic interface
  , null
  , length
  , append
  , map, map', imap'
  , foldl', ifoldl', foldl1'
  , foldr', ifoldr', foldr1'
    -- ** Special folds
  , concat
  , concatMap
  , maximum
  , minimum
  , product
  , sum
  , count
  -- * Building vector
  -- ** Accumulating maps
  , mapAccumL
  , mapAccumR
  -- ** Generating and unfolding vector
  , replicate
  , unfoldr
  , unfoldrN
  -- * Searching by equality
  , elem
  , elemIndex
  -- * Misc
  , IPair(..)
  , defaultInitSize
  , chunkOverhead
  , defaultChunkSize
  , smallChunkSize
  , VectorException(..)
  , errorEmptyVector
  , errorOutRange
 ) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.Data
import qualified Data.Foldable                 as F
import           Data.Functor.Identity
import qualified Data.List                     as List
import           Data.Maybe
import           Data.Monoid                   (Monoid (..))
import           Data.Primitive
import           Data.Primitive.PrimArray
import           Data.Primitive.SmallArray
import           Data.Semigroup                (Semigroup ((<>)))
import qualified Data.Traversable              as T
import           Data.Typeable
import           GHC.Exts                      (build)
import           GHC.Stack
import           GHC.Prim
import           GHC.Types
import           GHC.Word
import           Prelude                       hiding (concat, concatMap,
                                                elem, notElem, null, length, map,
                                                foldl, foldl1, foldr, foldr1,
                                                maximum, minimum, product, sum,
                                                replicate, traverse)
import           Std.Data.Array
import           Std.Data.PrimArray.BitTwiddle (c_memchr)

-- | Typeclass for box and unboxed vectors, which are created by slicing arrays.
--
class (Arr (MArray v) (IArray v) a) => Vec v a where
    -- | Vector's mutable array type
    type MArray v = (marr :: * -> * -> *) | marr -> v
    -- | Vector's immutable array type
    type IArray v = (iarr :: * -> *) | iarr -> v
    -- | Get underline array and slice range(offset and length).
    toArr :: v a -> (IArray v a, Int, Int)
    -- | Create a vector by slicing an array(with offset and length).
    fromArr :: IArray v a -> Int -> Int -> v a

-- | A pattern synonyms for matching the underline array, offset and length.
--
-- This is a bidirectional pattern synonyms, but very unsafe if not use properly.
-- Make sure your slice is within array's bounds!
pattern Vec :: Vec v a => IArray v a -> Int -> Int -> v a
pattern Vec arr s l <- (toArr -> (arr,s,l)) where
        Vec arr s l = fromArr arr s l

-- | /O(1)/ Index array element.
--
-- Return 'Nothing' if index is out of bounds.
--
(!) :: (Vec v a, HasCallStack) => v a -> Int -> Maybe a
{-# INLINE (!) #-}
infixl 9 !
(!) (Vec arr s l) i | i < 0 || i >= l = Nothing
                    | otherwise       = arr `indexArrM` (s + i)

--------------------------------------------------------------------------------
-- | Boxed vector
--
data Vector a = Vector
    {-# UNPACK #-} !(SmallArray a) -- payload
    {-# UNPACK #-} !Int         -- offset
    {-# UNPACK #-} !Int         -- length
    deriving (Typeable, Data)

instance Vec Vector a where
    type MArray Vector = SmallMutableArray
    type IArray Vector = SmallArray
    {-# INLINE toArr #-}
    toArr (Vector arr s l) = (arr, s, l)
    {-# INLINE fromArr #-}
    fromArr = Vector

instance Eq a => Eq (Vector a) where
    {-# INLINABLE (==) #-}
    v1 == v2 = eqVector v1 v2

eqVector :: Eq a => Vector a -> Vector a -> Bool
{-# INLINE eqVector #-}
eqVector (Vector baA sA lA) (Vector baB sB lB)
    | baA `sameArr` baB =
        if sA == sB then lA == lB else lA == lB && go sA sB
    | otherwise = lA == lB && go sA sB
  where
    !endA = sA + lA
    go !i !j
        | i >= endA = True
        | otherwise =
            (indexSmallArray baA i == indexSmallArray baB j) && go (i+1) (j+1)

instance Ord a => Ord (Vector a) where
    {-# INLINABLE compare #-}
    compare = compareVector

compareVector :: Ord a => Vector a -> Vector a -> Ordering
{-# INLINE compareVector #-}
compareVector (Vector baA sA lA) (Vector baB sB lB)
    | baA `sameArr` baB = if sA == sB then lA `compare` lB else go sA sB
    | otherwise = go sA sB
  where
    !endA = sA + lA
    !endB = sB + lB
    go !i !j | i >= endA  = endA `compare` endB
             | j >= endB  = endA `compare` endB
             | otherwise = let o = indexSmallArray baA i `compare` indexSmallArray baB j
                           in case o of EQ -> go (i+1) (j+1)
                                        x  -> x

instance Semigroup (Vector a) where
    {-# INLINE (<>) #-}
    (<>)    = append

instance Monoid (Vector a) where
    {-# INLINE mempty #-}
    mempty  = empty
    {-# INLINE mappend #-}
    mappend = append
    {-# INLINE mconcat #-}
    mconcat = concat

instance NFData a => NFData (Vector a) where
    {-# INLINE rnf #-}
    rnf (Vector arr s l) = go s
      where
        !end = s+l
        go !i | i < end   = case indexArr' arr i of (# x #) -> x `seq` go (i+1)
              | otherwise = ()

instance (Show a) => Show (Vector a) where
    showsPrec p v = showsPrec p (unpack v)

instance (Read a) => Read (Vector a) where
    readsPrec p str = [ (pack x, y) | (x, y) <- readsPrec p str ]

instance Functor Vector where
    {-# INLINE fmap #-}
    fmap = map

instance F.Foldable Vector where
    {-# INLINE foldr' #-}
    foldr' = foldr'
    {-# INLINE foldr #-}
    foldr f acc = List.foldr f acc . unpack
    {-# INLINE foldl' #-}
    foldl' = foldl'
    {-# INLINE foldl #-}
    foldl f acc = List.foldr (flip f) acc . unpackR
    {-# INLINE toList #-}
    toList = unpack
    {-# INLINE null #-}
    null = null
    {-# INLINE length #-}
    length = length
    {-# INLINE elem #-}
    elem = elem
    {-# INLINE maximum #-}
    maximum = maximum
    {-# INLINE minimum #-}
    minimum = minimum
    {-# INLINE product #-}
    product = product
    {-# INLINE sum #-}
    sum = sum

instance T.Traversable Vector where
    traverse = traverse

traverse :: (Vec v a, Vec u b, Applicative f) => (a -> f b) -> v a -> f (u b)
{-# INLINE [1] traverse #-}
{-# RULES "traverse/ST" traverse = traverseST #-}
{-# RULES "traverse/IO" traverse = traverseIO #-}
traverse f v = packN (length v) <$> T.traverse f (unpack v)

traverseST :: forall v u a b s. (Vec v a, Vec u b) => (a -> ST s b) -> v a -> ST s (u b)
{-# INLINE traverseST #-}
traverseST f (Vec arr s l)
    | l == 0    = return empty
    | otherwise = do
        mba <- newArr l
        go mba 0
        ba <- unsafeFreezeArr mba
        return $! fromArr ba 0 l
  where
    go :: MArray u s b -> Int -> ST s ()
    go !mba !i
        | i >= l = return ()
        | otherwise = do
            x <- indexArrM arr i
            writeArr mba i =<< f x
            go mba (i+1)

traverseIO :: forall v u a b. (Vec v a, Vec u b) => (a -> IO b) -> v a -> IO (u b)
{-# INLINE traverseIO #-}
traverseIO f (Vec arr s l)
    | l == 0    = return empty
    | otherwise = do
        mba <- newArr l
        go mba 0
        ba <- unsafeFreezeArr mba
        return $! fromArr ba 0 l
  where
    go :: MArray u RealWorld b -> Int -> IO ()
    go !mba !i
        | i >= l = return ()
        | otherwise = do
            x <- indexArrM arr i
            writeArr mba i =<< f x
            go mba (i+1)

--------------------------------------------------------------------------------
-- | Primitive vector
--
data PrimVector a = PrimVector
    {-# UNPACK #-} !(PrimArray a) -- payload
    {-# UNPACK #-} !Int         -- offset in elements of type a rather than in bytes
    {-# UNPACK #-} !Int         -- length in elements of type a rather than in bytes
  deriving Typeable

instance Prim a => Vec PrimVector a where
    type MArray PrimVector = MutablePrimArray
    type IArray PrimVector = PrimArray
    {-# INLINE toArr #-}
    toArr (PrimVector arr s l) = (arr, s, l)
    {-# INLINE fromArr #-}
    fromArr arr s l = PrimVector arr s l

instance (Prim a, Eq a) => Eq (PrimVector a) where
    {-# INLINE (==) #-}
    (==) = eqPrimVector

eqPrimVector :: forall a. Prim a => PrimVector a -> PrimVector a -> Bool
{-# INLINE eqPrimVector #-}
eqPrimVector (PrimVector (PrimArray baA#) (I# sA#) lA@(I# lA#))
             (PrimVector (PrimArray baB#) (I# sB#) lB@(I# lB#))
    = -- we use memcmp for all primitive vector, ghc emit code to test
      -- pointer equality so we don't have to do it manually here
      lA == lB &&
        0 == I# (compareByteArrays# baA# (sA# *# siz#) baB# (sB# *# siz#) n#)
  where
    siz@(I# siz#) = sizeOf (undefined :: a)
    (I# n#) = min (lA*siz) (lB*siz)

instance {-# OVERLAPPABLE #-} (Prim a, Ord a) => Ord (PrimVector a) where
    {-# INLINE compare #-}
    compare = comparePrimVector

instance {-# OVERLAPPING #-} Ord (PrimVector Word8) where
    {-# INLINE compare #-}
    compare = compareBytes

comparePrimVector :: (Prim a, Ord a) => PrimVector a -> PrimVector a -> Ordering
{-# INLINE comparePrimVector #-}
comparePrimVector (PrimVector baA sA lA) (PrimVector baB sB lB)
    | baA `sameArr` baB = if sA == sB then lA `compare` lB else go sA sB
    | otherwise = go sA sB
  where
    !endA = sA + lA
    !endB = sB + lB
    go !i !j | i >= endA  = endA `compare` endB
             | j >= endB  = endA `compare` endB
             | otherwise = let o = indexPrimArray baA i `compare` indexPrimArray baB j
                           in case o of EQ -> go (i+1) (j+1)
                                        x  -> x

compareBytes :: PrimVector Word8 -> PrimVector Word8 -> Ordering
{-# INLINE compareBytes #-}
compareBytes (PrimVector (PrimArray baA#) (I# sA#) lA@(I# lA#))
             (PrimVector (PrimArray baB#) (I# sB#) lB@(I# lB#)) =
    let (I# n#) = min lA lB
        r = I# (compareByteArrays# baA# sA# baB# sB# n#)
    in case r `compare` 0 of
        EQ  -> lA `compare` lB
        x  -> x

instance Prim a => Semigroup (PrimVector a) where
    {-# INLINE (<>) #-}
    (<>)    = append

instance Prim a => Monoid (PrimVector a) where
    {-# INLINE mempty #-}
    mempty  = empty
    {-# INLINE mappend #-}
    mappend = append
    {-# INLINE mconcat #-}
    mconcat = concat

instance NFData (PrimVector a) where
    {-# INLINE rnf #-}
    rnf PrimVector{} = ()

instance (Prim a, Show a) => Show (PrimVector a) where
    showsPrec p v = showsPrec p (unpack v)

instance (Prim a, Read a) => Read (PrimVector a) where
    readsPrec p str = [ (pack x, y) | (x, y) <- readsPrec p str ]

--------------------------------------------------------------------------------

-- | 'Bytes' is just primitive word8 vectors.
type Bytes = PrimVector Word8

pattern Bytes# :: ByteArray# -> Int# -> Int# -> PrimVector a
pattern Bytes# ba# s# l# = PrimVector (PrimArray ba#) (I# s#) (I# l#)

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
--
w2c :: Word8 -> Char
{-# INLINE w2c #-}
w2c (W8# w#) = C# (chr# (word2Int# w#))

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for PrimVector construction.
c2w :: Char -> Word8
{-# INLINE c2w #-}
c2w (C# c#) = W8# (int2Word# (ord# c#))

--------------------------------------------------------------------------------
-- Basic creating

-- | Create a vector with size N.
--
create :: Vec v a
       => Int                                   -- ^ length in elements of type @a@
       -> (forall s. MArray v s a -> ST s ())   -- ^ initialization function
       -> v a
{-# INLINE create #-}
create n0 fill = runST (do
        let n = max 0 n0
        mba <- newArr n
        fill mba
        ba <- unsafeFreezeArr mba
        return $! fromArr ba 0 n)

-- | Create a vector, return both the vector and the monadic result during creating.
--
creating :: Vec v a
         => Int  -- length in elements of type @a@
         -> (forall s. MArray v s a -> ST s b)  -- ^ initialization function
         -> (b, v a)
{-# INLINE creating #-}
creating n0 fill = runST (do
        let n = max 0 n0
        mba <- newArr n
        b <- fill mba
        ba <- unsafeFreezeArr mba
        let !v = fromArr ba 0 n
        return (b, v))

-- | Create a vector up to a specific length.
--
-- If the initialization function return a length larger than initial size,
-- an 'IndexOutOfVectorRange' will be raised.
--
createN :: (Vec v a, HasCallStack)
        => Int                                  -- ^ length's upper bound
        -> (forall s. MArray v s a -> ST s Int) -- ^ initialization function which return the actual length
        -> v a
{-# INLINE createN #-}
createN n0 fill = runST (do
        let n = max 0 n0
        mba <- newArr n
        l' <- fill mba
        shrinkMutableArr mba l'
        ba <- unsafeFreezeArr mba
        if l' <= n
        then return $! fromArr ba 0 l'
        else errorOutRange l')

-- | Create two vector up to a specific length.
--
-- If the initialization function return lengths larger than initial sizes,
-- an 'IndexOutOfVectorRange' will be raised.
--
createN2 :: (Vec v a, Vec u b, HasCallStack)
         => Int
         -> Int
         -> (forall s. MArray v s a -> MArray u s b -> ST s (Int,Int))
         -> (v a, u b)
{-# INLINE createN2 #-}
createN2 n0 n1 fill = runST (do
        let n0' = max 0 n0
            n1' = max 0 n1
        mba0 <- newArr n0'
        mba1 <- newArr n1'
        (l0, l1) <- fill mba0 mba1
        shrinkMutableArr mba0 l0
        shrinkMutableArr mba1 l1
        ba0 <- unsafeFreezeArr mba0
        ba1 <- unsafeFreezeArr mba1
        if (l0 <= n0)
        then if (l1 <= n1)
            then let !v1 = fromArr ba0 0 l0
                     !v2 = fromArr ba1 0 l1
                 in return (v1, v2)
            else errorOutRange l1
        else errorOutRange l0)

-- | /O(1)/. The empty vector.
--
empty :: Vec v a => v a
{-# INLINE empty #-}
empty = create 0 (\_ -> return ())

-- | /O(1)/. Single element vector.
singleton :: Vec v a => a -> v a
{-# INLINE singleton #-}
singleton c = create 1 (\ mba -> writeArr mba 0 c)

-- | /O(n)/. Copy a vector from slice.
--
copy :: Vec v a => v a -> v a
{-# INLINE copy #-}
copy (Vec ba s l) = create l (\ mba -> copyArr mba 0 ba s l)

--------------------------------------------------------------------------------
-- Conversion between list
--
-- | /O(n)/ Convert a list into a vector
--
-- Alias for @'packN' 'defaultInitSize'@.
--
pack :: Vec v a => [a] -> v a
{-# INLINE pack #-}
pack = packN defaultInitSize


-- | /O(n)/ Convert a list into a vector with an approximate size.
--
-- If the list's length is large than the size given, we simply double the buffer size
-- and continue building.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packN :: forall v a. Vec v a => Int -> [a] -> v a
{-# INLINE packN #-}
packN n0 = \ ws0 -> runST (do let n = max 4 n0
                              mba <- newArr n
                              (IPair i mba') <- foldM go (IPair 0 mba) ws0
                              shrinkMutableArr mba' i
                              ba <- unsafeFreezeArr mba'
                              return $! fromArr ba 0 i
                          )
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: IPair (MArray v s a) -> a -> ST s (IPair (MArray v s a))
    go (IPair i mba) x = do
        n <- sizeofMutableArr mba
        if i < n
        then do writeArr mba i x
                return (IPair (i+1) mba)
        else do let !n' = n `shiftL` 1
                !mba' <- resizeMutableArr mba n'
                writeArr mba' i x
                return (IPair (i+1) mba')

-- | /O(n)/
--
-- Alias for @'packRN' 'defaultInitSize'@.
--
packR :: Vec v a => [a] -> v a
{-# INLINE packR #-}
packR = packRN defaultInitSize

-- | /O(n)/ 'packN' in reverse order.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packRN :: forall v a. Vec v a => Int -> [a] -> v a
{-# INLINE packRN #-}
packRN n0 = \ ws0 -> runST (do let n = max 4 n0
                               mba <- newArr n
                               (IPair i mba') <- foldM go (IPair (n-1) mba) ws0
                               ba <- unsafeFreezeArr mba'
                               let i' = i + 1
                                   n' = sizeofArr ba
                               return $! fromArr ba i' (n'-i')
                           )
  where
    go :: IPair (MArray v s a) -> a -> ST s (IPair (MArray v s a))
    go (IPair i mba) !x = do
        n <- sizeofMutableArr mba
        if i >= 0
        then do writeArr mba i x
                return (IPair (i-1) mba)
        else do let !n' = n `shiftL` 1  -- double the buffer
                !mba' <- newArr n'
                copyMutableArr mba' n mba 0 n
                writeArr mba' (n-1) x
                return (IPair (n-2) mba')

-- | /O(n)/ Convert vector to a list.
--
-- Unpacking is done lazily. i.e. we will retain reference to the array until all element are consumed.
--
-- This function is a /good producer/ in the sense of build/foldr fusion.
unpack :: Vec v a => v a -> [a]
{-# INLINE [1] unpack #-}
unpack (Vec ba s l) = go s
  where
    !end = s + l
    go !idx
        | idx >= end = []
        | otherwise = case indexArr' ba idx of (# x #) -> x : go (idx+1)

unpackFB :: Vec v a => v a -> (a -> r -> r) -> r -> r
{-# INLINE [0] unpackFB #-}
unpackFB (Vec ba s l) k z = go s
  where
    !end = s + l
    go !idx
        | idx >= end = z
        | otherwise = case indexArr' ba idx of (# x #) -> x `k` go (idx+1)

{-# RULES
"unpack" [~1] forall v . unpack v = build (\ k z -> unpackFB v k z)
"unpackFB" [1] forall v . unpackFB v (:) [] = unpack v
 #-}

-- | /O(n)/ Convert vector to a list in reverse order.
--
-- This function is a /good producer/ in the sense of build/foldr fusion.
unpackR :: Vec v a => v a -> [a]
{-# INLINE [1] unpackR #-}
unpackR (Vec ba s l) = go (s + l - 1)
  where
    go !idx
        | idx < s = []
        | otherwise =
            case indexArr' ba idx of (# x #) -> x : go (idx-1)

unpackRFB :: Vec v a => v a -> (a -> r -> r) -> r -> r
{-# INLINE [0] unpackRFB #-}
unpackRFB (Vec ba s l) k z = go (s + l - 1)
  where
    go !idx
        | idx < s = z
        | otherwise =
            case indexArr' ba idx of (# x #) -> x `k` go (idx-1)

{-# RULES
"unpackR" [~1] forall v . unpackR v = build (\ k z -> unpackRFB v k z)
"unpackRFB" [1] forall v . unpackRFB v (:) [] = unpackR v
 #-}

--------------------------------------------------------------------------------
-- Basic interface
--
-- |  /O(1)/ The length of a vector.
length :: Vec v a => v a -> Int
{-# INLINE length #-}
length (Vec _ _ l) = l

-- | /O(1)/ Test whether a vector is empty.
null :: Vec v a => v a -> Bool
{-# INLINE null #-}
null v = length v == 0

-- | /O(m+n)/
--
-- There's no need to guard empty vector because we guard them for you, so
-- appending empty vectors are no-ops.
append :: Vec v a => v a -> v a -> v a
{-# INLINE append #-}
append (Vec _ _ 0) b                    = b
append a                (Vec _ _ 0)     = a
append (Vec baA sA lA) (Vec baB sB lB) = create (lA+lB) $ \ mba -> do
    copyArr mba 0  baA sA lA
    copyArr mba lA baB sB lB

--------------------------------------------------------------------------------

-- | Mapping between vectors (possiblely with two different vector types).
--
-- NOTE, the result vector contain thunks in lifted 'Vector' case, use 'map''
-- if that's not desired.
--
-- For 'PrimVector', 'map' and 'map'' are same, since 'PrimVector's never
-- store thunks.
map :: forall u v a b. (Vec u a, Vec v b) => (a -> b) -> u a -> v b
{-# INLINE map #-}
map f (Vec arr s l) = create l (go 0)
  where
    go :: Int -> MArray v s b -> ST s ()
    go !i !mba | i >= l = return ()
               | otherwise = do
                    x <- indexArrM arr (i+s); writeArr mba i (f x);
                    go (i+1) mba

-- | Mapping between vectors (possiblely with two different vector types).
--
-- This is the strict version map. Note that the 'Functor' instance of lifted
-- 'Vector' is defined with 'map' to statisfy laws, which this strict version
-- breaks (@map' id arrayContainsBottom /= arrayContainsBottom @).
map' :: forall u v a b. (Vec u a, Vec v b) => (a -> b) -> u a -> v b
{-# INLINE map' #-}
map' f (Vec arr s l) = create l (go 0)
  where
    go :: Int -> MArray v s b -> ST s ()
    go !i !mba | i < l = do
                    x <- indexArrM arr (i+s)
                    let !v = f x in writeArr mba i v
                    go (i+1) mba
               | otherwise = return ()

-- | Strict mapping with index.
--
imap' :: forall u v a b. (Vec u a, Vec v b) => (Int -> a -> b) -> u a -> v b
{-# INLINE imap' #-}
imap' f (Vec arr s l) = create l (go 0)
  where
    go :: Int -> MArray v s b -> ST s ()
    go !i !mba | i < l = do
                    x <- indexArrM arr (i+s)
                    let !v = f i x in writeArr mba i v
                    go (i+1) mba
               | otherwise = return ()

--------------------------------------------------------------------------------
--
-- Strict folds
--

-- | Strict left to right fold.
foldl' :: Vec v a => (b -> a -> b) -> b -> v a -> b
{-# INLINE foldl' #-}
foldl' f z (Vec arr s l) = go z s
  where
    !end = s + l
    -- tail recursive; traverses array left to right
    go !acc !i | i < end  = case indexArr' arr i of
                                (# x #) -> go (f acc x) (i + 1)
               | otherwise = acc

-- | Strict left to right fold with index.
ifoldl' :: Vec v a => (b -> Int ->  a -> b) -> b -> v a -> b
{-# INLINE ifoldl' #-}
ifoldl' f z (Vec arr s l) = go z s
  where
    !end = s + l
    go !acc !i | i < end  = case indexArr' arr i of
                                (# x #) -> go (f acc i x) (i + 1)
               | otherwise = acc

-- | Strict left to right fold using first element as the initial value.
foldl1' :: forall v a. (Vec v a, HasCallStack) => (a -> a -> a) -> v a -> a
{-# INLINE foldl1' #-}
foldl1' f (Vec arr s l)
    | l <= 0    = errorEmptyVector "foldl1'"
    | otherwise = case indexArr' arr s of
                    (# x0 #) -> foldl' f x0 (fromArr arr (s+1) (l-1) :: v a)

-- | Strict right to left fold
foldr' :: Vec v a => (a -> b -> b) -> b -> v a -> b
{-# INLINE foldr' #-}
foldr' f z (Vec arr s l) = go z (s+l-1)
  where
    -- tail recursive; traverses array right to left
    go !acc !i | i >= s    = case indexArr' arr i of
                                (# x #) -> go (f x acc) (i - 1)
               | otherwise = acc

-- | Strict right to left fold with index
ifoldr' :: Vec v a => (Int -> a -> b -> b) -> b -> v a -> b
{-# INLINE ifoldr' #-}
ifoldr' f z (Vec arr s l) = go z (s+l-1)
  where
    go !acc !i | i >= s    = case indexArr' arr i of
                                (# x #) -> go (f i x acc) (i - 1)
               | otherwise = acc

-- | Strict right to left fold using last element as the initial value.
foldr1' :: forall v a. (Vec v a, HasCallStack) => (a -> a -> a) -> v a -> a
{-# INLINE foldr1' #-}
foldr1' f (Vec arr s l)
    | l <= 0 = errorEmptyVector "foldr1'"
    | otherwise = case indexArr' arr (s+l-1) of
                    (# x0 #) -> foldl' f x0 (fromArr arr s (l-1) :: v a)

--------------------------------------------------------------------------------
--
-- Special folds
--
-- | /O(n)/ Concatenate a list of vector.
--
-- Note: 'concat' have to force the entire list to filter out empty vector and calculate
-- the length for allocation.
concat :: forall v a . Vec v a => [v a] -> v a
{-# INLINE concat #-}
concat [v] = v  -- shortcut common case in Parser
concat vs = case pre 0 0 vs of
    (1, _) -> let Just v = List.find (not . null) vs in v -- there must be a not null vector
    (_, l) -> create l (copy vs 0)
  where
    -- pre scan to decide if we really need to copy and calculate total length
    -- we don't accumulate another result list, since it's rare to got empty
    pre :: Int -> Int -> [v a] -> (Int, Int)
    pre !nacc !lacc [] = (nacc, lacc)
    pre !nacc !lacc (v@(Vec _ _ l):vs)
        | l <= 0    = pre nacc lacc vs
        | otherwise = pre (nacc+1) (l+lacc) vs

    copy :: [v a] -> Int -> MArray v s a -> ST s ()
    copy [] !_ !_                   = return ()
    copy (Vec ba s l:vs) !i !mba = do when (l /= 0) (copyArr mba i ba s l)
                                      copy vs (i+l) mba

-- | Map a function over a vector and concatenate the results
concatMap :: Vec v a => (a -> v a) -> v a -> v a
{-# INLINE concatMap #-}
concatMap f = concat . foldr' ((:) . f) []

-- | /O(n)/ 'maximum' returns the maximum value from a vector
--
-- It's defined with 'foldl1'', an 'EmptyVector' exception will be thrown
-- in the case of an empty vector.
maximum :: (Vec v a, Ord a, HasCallStack) => v a -> a
{-# INLINE maximum #-}
maximum = foldl1' max

-- | /O(n)/ 'minimum' returns the minimum value from a 'vector'
--
-- An 'EmptyVector' exception will be thrown in the case of an empty vector.
minimum :: (Vec v a, Ord a, HasCallStack) => v a -> a
{-# INLINE minimum #-}
minimum = foldl1' min

-- | /O(n)/ 'product' returns the product value from a vector
--
-- An 'EmptyVector' exception will be thrown in the case of an empty vector.
product :: (Vec v a, Num a, HasCallStack) => v a -> a
{-# INLINE product #-}
product = foldl1' (*)

-- | /O(n)/ 'sum' returns the sum value from a 'vector'
--
-- An 'EmptyVector' exception will be thrown in the case of an empty vector.
sum :: (Vec v a, Num a, HasCallStack) => v a -> a
{-# INLINE sum #-}
sum = foldl1' (+)

-- | /O(n)/ 'count' returns count of an element from a 'vector'
--
count :: (Vec v a, Eq a) => a -> v a -> Int
{-# INLINE count #-}
count w = foldl' (\ acc x -> if x == w then acc+1 else acc) 0

--------------------------------------------------------------------------------
-- Accumulating maps

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a vector,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new list.
--
-- Note, this function will only force the result tuple, not the elements inside,
-- to prevent creating thunks during 'mapAccumL', `seq` your accumulator and result
-- with the result tuple.
--
mapAccumL :: forall u v a b c. (Vec u b, Vec v c) => (a -> b -> (a, c)) -> a -> u b -> (a, v c)
{-# INLINE mapAccumL #-}
mapAccumL f z (Vec ba s l)
    | l <= 0    = (z, empty)
    | otherwise = creating l (go z s)
  where
    !end = s + l
    go :: a -> Int -> MArray v s c -> ST s a
    go acc !i !mba
        | i >= end = return acc
        | otherwise = do
            x <- indexArrM ba i
            let (acc', c) = acc `f` x
            writeArr mba (i-s) c
            go acc' (i+1) mba

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a vector,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new vector.
--
-- The same strictness property with 'mapAccumL' applys to 'mapAccumR' too.
--
mapAccumR :: forall u v a b c. (Vec u b, Vec v c) => (a -> b -> (a, c)) -> a -> u b -> (a, v c)
{-# INLINE mapAccumR #-}
mapAccumR f z (Vec ba s l)
    | l <= 0    = (z, empty)
    | otherwise = creating l (go z (s+l-1))
  where
    go :: a -> Int ->  MArray v s c -> ST s a
    go acc !i !mba
        | i < s     = return acc
        | otherwise = do
            x <- indexArrM ba i
            let (acc', c) = acc `f` x
            writeArr mba (i-s) c
            go acc' (i-1) mba

--------------------------------------------------------------------------------
--  Generating and unfolding vector.
--
-- | /O(n)/ 'replicate' @n x@ is a vector of length @n@ with @x@
-- the value of every element.
--
-- Note: 'replicate' will not force the element.
--
replicate :: (Vec v a) => Int -> a -> v a
{-# INLINE replicate #-}
replicate n x = create n (\ mba -> setArr mba 0 n x)

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- vector from a seed value. The function takes the element and
-- returns 'Nothing' if it is done producing the vector or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
unfoldr :: Vec u b => (a -> Maybe (b, a)) -> a -> u b
{-# INLINE unfoldr #-}
unfoldr f = pack . List.unfoldr f

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a vector from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
unfoldrN :: forall v a b. Vec v b => Int -> (a -> Maybe (b, a)) -> a -> (v b, Maybe a)
{-# INLINE unfoldrN #-}
unfoldrN n f
    | n < 0     = \ z -> (empty, Just z)
    | otherwise = \ z ->
        let ((r, len), Vec arr _ _) = creating n (go z 0)
        in (Vec arr 0 len, r)
  where
    go :: a -> Int -> MArray v s b -> ST s (Maybe a, Int)
    go !acc !i !mba
      | n == i    = return (Just acc, i)
      | otherwise = case f acc of
          Nothing        -> return (Nothing, i)
          Just (x, acc') -> do writeArr mba i x
                               go acc' (i+1) mba

--------------------------------------------------------------------------------
-- Searching by equality

elem :: (Vec v a, Eq a) => a -> v a -> Bool
{-# INLINE elem #-}
elem x = isJust . elemIndex x

notElem ::  (Vec v a, Eq a) => a -> v a -> Bool
{-# INLINE notElem #-}
notElem a = not . elem a

elemIndex :: (Vec v a, Eq a) => a -> v a -> Maybe Int
{-# INLINE [1] elemIndex #-}
{-# RULES "elemIndex/Bytes" elemIndex = elemIndexBytes #-}
elemIndex w (Vec arr s l) = go s
  where
    !end = s + l
    go !i
        | i >= end = Nothing
        | x == w   = let !i' = i - s in Just i'
        | otherwise = go (i+1)
        where (# x #) = indexArr' arr i

elemIndexBytes :: Word8 -> Bytes -> Maybe Int
{-# INLINE elemIndexBytes #-}
elemIndexBytes w (PrimVector (PrimArray ba#) s l) =
    case fromIntegral (c_memchr ba# s w l) of
        -1 -> Nothing
        r  -> Just r

--------------------------------------------------------------------------------

-- | Index pair type to help GHC unpack in some loops.
--
data IPair a = IPair {-# UNPACK #-}!Int a

-- | The chunk size used for I\/O. Currently set to @32k-chunkOverhead@
defaultChunkSize :: Int
{-# INLINE defaultChunkSize #-}
defaultChunkSize = 32 * 1024 - chunkOverhead

-- | The recommended chunk size. Currently set to @4k - chunkOverhead@.
smallChunkSize :: Int
{-# INLINE smallChunkSize #-}
smallChunkSize = 4 * 1024 - chunkOverhead

-- | The memory management overhead. Currently this is tuned for GHC only.
chunkOverhead :: Int
{-# INLINE chunkOverhead #-}
chunkOverhead = 2 * sizeOf (undefined :: Int)

-- | @defaultInitSize = 30@, used as initialize size when packing list of unknown size.
defaultInitSize :: Int
{-# INLINE defaultInitSize #-}
defaultInitSize = 30

data VectorException = IndexOutOfVectorRange {-# UNPACK #-} !Int CallStack
                     | EmptyVector CallStack
                    deriving (Show, Typeable)
instance Exception VectorException

errorEmptyVector :: HasCallStack => a
{-# NOINLINE errorEmptyVector #-}
errorEmptyVector = throw (EmptyVector callStack)

errorOutRange :: HasCallStack => Int -> a
{-# NOINLINE errorOutRange #-}
errorOutRange i = throw (IndexOutOfVectorRange i callStack)
