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
{-# LANGUAGE UnliftedFFITypes       #-}
{-# LANGUAGE QuantifiedConstraints  #-}

{-|
Module      : Std.Data.Vector.Base
Description : Fast boxed and unboxed vector
Copyright   : (c) Dong Han, 2017-2019
              (c) Tao He, 2018-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides unified vector interface. Conceptually a vector is simply a slice of an array, for example this is the definition of boxed vector:

@
data Vector a = Vector !(SmallArray a)   !Int    !Int
                     -- payload           offset  length
@

The 'Vec' class unified different type of vectors, and this module provide operation over 'Vec' instances, with all the internal structures. Be careful on modifying internal slices, otherwise segmentation fault await.

-}

module Std.Data.Vector.Base (
  -- * The Vec typeclass
    Vec(..)
  , pattern Vec
  , indexMaybe
  -- * Boxed and unboxed vector type
  , Vector(..)
  , PrimVector(..)
  -- ** Word8 vector
  , Bytes, packASCII
  , w2c, c2w
  -- * Basic creating
  , create, create', creating, creating', createN, createN2
  , empty, singleton, copy
  -- * Conversion between list
  , pack, packN, packR, packRN
  , unpack, unpackR
  -- * Basic interface
  , null
  , length
  , append
  , map, map', imap', traverseVec, traverseWithIndex, traverseVec_, traverseWithIndex_
  , foldl', ifoldl', foldl1', foldl1Maybe'
  , foldr', ifoldr', foldr1', foldr1Maybe'
    -- ** Special folds
  , concat, concatMap
  , maximum, minimum
  , maximumMaybe, minimumMaybe
  , sum
  , count
  , product, product'
  , all, any
  -- * Building vector
  -- ** Accumulating maps
  , mapAccumL
  , mapAccumR
  -- ** Generating and unfolding vector
  , replicate
  , cycleN
  , unfoldr
  , unfoldrN
  -- * Searching by equality
  , elem, notElem, elemIndex
  -- * Misc
  , IPair(..), mapIPair'
  , defaultInitSize
  , chunkOverhead
  , defaultChunkSize
  , smallChunkSize
  , VectorException(..)
  , errorEmptyVector
  , errorOutRange
  , castVector
  -- * C FFI
  , c_strcmp
  , c_strlen
  , c_ascii_validate_addr
  , c_fnv_hash_addr
  , c_fnv_hash_ba
 ) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.Char                     (ord)
import           Data.Data
import qualified Data.Foldable                 as F
import           Data.Functor.Identity
import           Data.Hashable                 (Hashable(..), hashByteArrayWithSalt)
import           Data.Hashable.Lifted          (Hashable1(..), hashWithSalt1)
import qualified Data.List                     as List
import           Data.Maybe
import           Data.Monoid                   (Monoid (..))
import qualified Data.CaseInsensitive          as CI
import           Data.Primitive
import           Data.Primitive.PrimArray
import           Data.Primitive.SmallArray
import           Data.Primitive.Ptr
import           Data.Semigroup                (Semigroup ((<>)))
import           Data.String                   (IsString(..))
import qualified Data.Traversable              as T
import           Data.Typeable
import           Foreign.C
import           GHC.CString
import           GHC.Exts                      (build)
import           GHC.Stack
import           GHC.Prim
import           GHC.Ptr
import           GHC.Types
import           GHC.Word
import           Prelude                       hiding (concat, concatMap,
                                                elem, notElem, null, length, map,
                                                foldl, foldl1, foldr, foldr1,
                                                maximum, minimum, product, sum,
                                                all, any, replicate, traverse)
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))
import           System.IO.Unsafe              (unsafeDupablePerformIO)

import           Std.Data.Array
import           Std.Data.PrimArray.BitTwiddle (c_memchr)
import           Std.Data.PrimArray.Cast

-- | Typeclass for box and unboxed vectors, which are created by slicing arrays.
--
-- Instead of providing a generalized vector with polymorphric array field, we use this typeclass
-- so that instances use concrete array type can unpack their array payload.
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
indexMaybe :: (Vec v a, HasCallStack) => v a -> Int -> Maybe a
{-# INLINE indexMaybe #-}
indexMaybe (Vec arr s l) i | i < 0 || i >= l = Nothing
                           | otherwise       = arr `indexArrM` (s + i)

--------------------------------------------------------------------------------
-- | Boxed vector
--
data Vector a = Vector
    {-# UNPACK #-} !(SmallArray a)  -- ^ payload
    {-# UNPACK #-} !Int             -- ^ offset
    {-# UNPACK #-} !Int             -- ^ length
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
    {-# INLINE traverse #-}
    traverse = traverseVec

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = pack <$> arbitrary
    shrink v = pack <$> shrink (unpack v)

instance CoArbitrary a => CoArbitrary (Vector a) where
    coarbitrary = coarbitrary . unpack

instance Hashable a => Hashable (Vector a) where
    {-# INLINE hashWithSalt #-}
    hashWithSalt = hashWithSalt1

instance Hashable1 Vector where
    {-# INLINE liftHashWithSalt #-}
    liftHashWithSalt h salt (Vector arr s l) = hashWithSalt (go salt s) l
      where
        !end = s + l
        go !salt !i
            | i >= end  = salt
            | otherwise = go (h salt (indexArr arr i)) (i+1)

traverseVec :: (Vec v a, Vec u b, Applicative f) => (a -> f b) -> v a -> f (u b)
{-# INLINE [1] traverseVec #-}
{-# RULES "traverseVec/ST" forall f. traverseVec f = traverseWithIndexST (const f) #-}
{-# RULES "traverseVec/IO" forall f. traverseVec f = traverseWithIndexIO (const f) #-}
traverseVec f v = packN (length v) <$> T.traverse f (unpack v)

traverseWithIndex :: (Vec v a, Vec u b, Applicative f) => (Int -> a -> f b) -> v a -> f (u b)
{-# INLINE [1] traverseWithIndex #-}
{-# RULES "traverseWithIndex/ST" traverseWithIndex = traverseWithIndexST #-}
{-# RULES "traverseWithIndex/IO" traverseWithIndex = traverseWithIndexIO #-}
traverseWithIndex f v = packN (length v) <$> zipWithM f [0..] (unpack v)

traverseWithIndexST :: forall v u a b s. (Vec v a, Vec u b) => (Int -> a -> ST s b) -> v a -> ST s (u b)
{-# INLINE traverseWithIndexST #-}
traverseWithIndexST f (Vec arr s l)
    | l == 0    = return empty
    | otherwise = do
        marr <- newArr l
        go marr 0
        ba <- unsafeFreezeArr marr
        return $! fromArr ba 0 l
  where
    go :: MArray u s b -> Int -> ST s ()
    go !marr !i
        | i >= l = return ()
        | otherwise = do
            x <- indexArrM arr (i+s)
            writeArr marr i =<< f i x
            go marr (i+1)

traverseWithIndexIO :: forall v u a b. (Vec v a, Vec u b) => (Int -> a -> IO b) -> v a -> IO (u b)
{-# INLINE traverseWithIndexIO #-}
traverseWithIndexIO f (Vec arr s l)
    | l == 0    = return empty
    | otherwise = do
        marr <- newArr l
        go marr 0
        ba <- unsafeFreezeArr marr
        return $! fromArr ba 0 l
  where
    go :: MArray u RealWorld b -> Int -> IO ()
    go !marr !i
        | i >= l = return ()
        | otherwise = do
            x <- indexArrM arr (i+s)
            writeArr marr i =<< f i x
            go marr (i+1)

traverseVec_ :: (Vec v a, Applicative f) => (a -> f b) -> v a -> f ()
{-# INLINE traverseVec_ #-}
traverseVec_ f = traverseWithIndex_ (\ _ x -> f x)

traverseWithIndex_ :: (Vec v a, Applicative f) => (Int -> a -> f b) -> v a -> f ()
{-# INLINE traverseWithIndex_ #-}
traverseWithIndex_ f (Vec arr s l) = go s
  where
    end = s + l
    go !i
        | i >= l = pure ()
        | otherwise = f (i-s) (indexArr arr i) *> go (i+1)

--------------------------------------------------------------------------------
-- | Primitive vector
--
data PrimVector a = PrimVector
    {-# UNPACK #-} !(PrimArray a)   -- ^ payload
    {-# UNPACK #-} !Int             -- ^ offset in elements of type a rather than in bytes
    {-# UNPACK #-} !Int             -- ^ length in elements of type a rather than in bytes
  deriving Typeable

instance Prim a => Vec PrimVector a where
    type MArray PrimVector = MutablePrimArray
    type IArray PrimVector = PrimArray
    {-# INLINE toArr #-}
    toArr (PrimVector arr s l) = (arr, s, l)
    {-# INLINE fromArr #-}
    fromArr = PrimVector

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

instance (Prim a, Ord a) => Ord (PrimVector a) where
    {-# INLINE compare #-}
    compare = comparePrimVector


comparePrimVector :: (Prim a, Ord a) => PrimVector a -> PrimVector a -> Ordering
{-# INLINE [1] comparePrimVector #-}
{-# RULES
    "comparePrimVector/Bytes" comparePrimVector = compareBytes
  #-}
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

instance (Prim a, Arbitrary a) => Arbitrary (PrimVector a) where
    arbitrary = pack <$> arbitrary
    shrink v = pack <$> shrink (unpack v)

instance (Prim a, CoArbitrary a) => CoArbitrary (PrimVector a) where
    coarbitrary = coarbitrary . unpack

instance (Hashable a, Prim a) => Hashable (PrimVector a) where
    {-# INLINE hashWithSalt #-}
    hashWithSalt = hashWithSaltPrimVector

hashWithSaltPrimVector :: (Hashable a, Prim a) => Int -> PrimVector a -> Int
{-# INLINE [1] hashWithSaltPrimVector #-}
{-# RULES
    "hashWithSaltPrimVector/Bytes" hashWithSaltPrimVector = hashWithSaltBytes
  #-}
hashWithSaltPrimVector salt (PrimVector arr s l) = go salt s
  where
    -- we don't do a final hash with length to keep consistent with Bytes's instance
    !end = s + l
    go !salt !i
        | i >= end  = salt
        | otherwise = go (hashWithSalt salt (indexPrimArray arr i)) (i+1)

hashWithSaltBytes :: Int -> Bytes -> Int
{-# INLINE hashWithSaltBytes #-}
hashWithSaltBytes salt (PrimVector (PrimArray ba#) s l) =
    unsafeDupablePerformIO (c_fnv_hash_ba ba# s l salt)

--------------------------------------------------------------------------------

-- | 'Bytes' is just primitive word8 vectors.
type Bytes = PrimVector Word8

instance (a ~ Word8) => IsString (PrimVector a) where
    {-# INLINE fromString #-}
    fromString = packASCII

instance CI.FoldCase Bytes where
    {-# INLINE foldCase #-}
    foldCase = map toLower8
      where
        toLower8 :: Word8 -> Word8
        toLower8 w
          |  65 <= w && w <=  90 ||
            192 <= w && w <= 214 ||
            216 <= w && w <= 222 = w + 32
          | otherwise            = w

packASCII :: String -> Bytes
{-# INLINE CONLIKE [0] packASCII #-}
{-# RULES
    "packASCII/packASCIIAddr" forall addr . packASCII (unpackCString# addr) = packASCIIAddr addr
  #-}
packASCII = pack . fmap (fromIntegral . ord)

packASCIIAddr :: Addr# -> Bytes
packASCIIAddr addr# = copy addr#
  where
    len = fromIntegral . unsafeDupablePerformIO $ c_strlen addr#
    copy addr# = runST $ do
        marr <- newPrimArray len
        copyPtrToMutablePrimArray marr 0 (Ptr addr#) len
        arr <- unsafeFreezePrimArray marr
        return (PrimVector arr 0 len)

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
        marr <- newArr n
        fill marr
        ba <- unsafeFreezeArr marr
        return $! fromArr ba 0 n)

-- | Create a vector with a initial size N array (which may not be the final array).
--
create' :: Vec v a
        => Int                                                      -- ^ length in elements of type @a@
        -> (forall s. MArray v s a -> ST s (IPair (MArray v s a)))  -- ^ initialization function
                                                                    --   return a result size and array
                                                                    --   the result must start from index 0
        -> v a
{-# INLINE create' #-}
create' n0 fill = runST (do
        let n = max 0 n0
        marr <- newArr n
        IPair n' marr' <- fill marr
        shrinkMutableArr marr' n'
        ba <- unsafeFreezeArr marr'
        return $! fromArr ba 0 n')

-- | Create a vector with a initial size N array, return both the vector and
-- the monadic result during creating.
--
-- The result is not demanded strictly while the returned vector will be in normal form.
-- It this is not desired, use @return $!@ idiom in your initialization function.
creating :: Vec v a
         => Int  -- length in elements of type @a@
         -> (forall s. MArray v s a -> ST s b)  -- ^ initialization function
         -> (b, v a)
{-# INLINE creating #-}
creating n0 fill = runST (do
        let n = max 0 n0
        marr <- newArr n
        b <- fill marr
        ba <- unsafeFreezeArr marr
        let !v = fromArr ba 0 n
        return (b, v))

-- | Create a vector with a initial size N array (which may not be the final array),
-- return both the vector and the monadic result during creating.
--
-- The result is not demanded strictly while the returned vector will be in normal form.
-- It this is not desired, use @return $!@ idiom in your initialization function.
creating' :: Vec v a
         => Int  -- length in elements of type @a@
         -> (forall s. MArray v s a -> ST s (b, (IPair (MArray v s a))))  -- ^ initialization function
         -> (b, v a)
{-# INLINE creating' #-}
creating' n0 fill = runST (do
        let n = max 0 n0
        marr <- newArr n
        (b, IPair n' marr') <- fill marr
        shrinkMutableArr marr' n'
        ba <- unsafeFreezeArr marr'
        let !v = fromArr ba 0 n'
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
        marr <- newArr n
        l' <- fill marr
        shrinkMutableArr marr l'
        ba <- unsafeFreezeArr marr
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
singleton c = create 1 (\ marr -> writeArr marr 0 c)

-- | /O(n)/. Copy a vector from slice.
--
copy :: Vec v a => v a -> v a
{-# INLINE copy #-}
copy (Vec ba s l) = create l (\ marr -> copyArr marr 0 ba s l)

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
                              marr <- newArr n
                              (IPair i marr') <- foldM go (IPair 0 marr) ws0
                              shrinkMutableArr marr' i
                              ba <- unsafeFreezeArr marr'
                              return $! fromArr ba 0 i
                          )
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: IPair (MArray v s a) -> a -> ST s (IPair (MArray v s a))
    go (IPair i marr) x = do
        n <- sizeofMutableArr marr
        if i < n
        then do writeArr marr i x
                return (IPair (i+1) marr)
        else do let !n' = n `shiftL` 1
                !marr' <- resizeMutableArr marr n'
                writeArr marr' i x
                return (IPair (i+1) marr')

-- | /O(n)/ Alias for @'packRN' 'defaultInitSize'@.
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
                               marr <- newArr n
                               (IPair i marr') <- foldM go (IPair (n-1) marr) ws0
                               ba <- unsafeFreezeArr marr'
                               let i' = i + 1
                                   n' = sizeofArr ba
                               return $! fromArr ba i' (n'-i')
                           )
  where
    go :: IPair (MArray v s a) -> a -> ST s (IPair (MArray v s a))
    go (IPair i marr) !x = do
        n <- sizeofMutableArr marr
        if i >= 0
        then do writeArr marr i x
                return (IPair (i-1) marr)
        else do let !n' = n `shiftL` 1  -- double the buffer
                !marr' <- newArr n'
                copyMutableArr marr' n marr 0 n
                writeArr marr' (n-1) x
                return (IPair (n-2) marr')

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
append (Vec baA sA lA) (Vec baB sB lB) = create (lA+lB) $ \ marr -> do
    copyArr marr 0  baA sA lA
    copyArr marr lA baB sB lB

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
    go !i !marr | i >= l = return ()
                | otherwise = do
                    x <- indexArrM arr (i+s); writeArr marr i (f x);
                    go (i+1) marr

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
    go !i !marr | i < l = do
                    x <- indexArrM arr (i+s)
                    let !v = f x in writeArr marr i v
                    go (i+1) marr
               | otherwise = return ()

-- | Strict mapping with index.
--
imap' :: forall u v a b. (Vec u a, Vec v b) => (Int -> a -> b) -> u a -> v b
{-# INLINE imap' #-}
imap' f (Vec arr s l) = create l (go 0)
  where
    go :: Int -> MArray v s b -> ST s ()
    go !i !marr | i < l = do
                    x <- indexArrM arr (i+s)
                    let !v = f i x in writeArr marr i v
                    go (i+1) marr
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
--
-- Throw 'EmptyVector' if vector is empty.
foldl1' :: forall v a. (Vec v a, HasCallStack) => (a -> a -> a) -> v a -> a
{-# INLINE foldl1' #-}
foldl1' f (Vec arr s l)
    | l <= 0    = errorEmptyVector
    | otherwise = case indexArr' arr s of
                    (# x0 #) -> foldl' f x0 (fromArr arr (s+1) (l-1) :: v a)

-- | Strict left to right fold using first element as the initial value.
--   return 'Nothing' when vector is empty.
foldl1Maybe' :: forall v a. Vec v a => (a -> a -> a) -> v a -> Maybe a
{-# INLINE foldl1Maybe' #-}
foldl1Maybe' f (Vec arr s l)
    | l <= 0    = Nothing
    | otherwise = case indexArr' arr s of
                    (# x0 #) -> let !r = foldl' f x0 (fromArr arr (s+1) (l-1) :: v a)
                                in Just r

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
--
-- NOTE: the index is counting from 0, not backwards
ifoldr' :: Vec v a => (Int -> a -> b -> b) -> b -> v a -> b
{-# INLINE ifoldr' #-}
ifoldr' f z (Vec arr s l) = go z (s+l-1) 0
  where
    go !acc !i !k | i >= s    = case indexArr' arr i of
                                    (# x #) -> go (f k x acc) (i - 1) (k + 1)
                  | otherwise = acc

-- | Strict right to left fold using last element as the initial value.
--
-- Throw 'EmptyVector' if vector is empty.
foldr1' :: forall v a. (Vec v a, HasCallStack) => (a -> a -> a) -> v a -> a
{-# INLINE foldr1' #-}
foldr1' f (Vec arr s l)
    | l <= 0 = errorEmptyVector
    | otherwise = case indexArr' arr (s+l-1) of
                    (# x0 #) -> foldl' f x0 (fromArr arr s (l-1) :: v a)

-- | Strict right to left fold using last element as the initial value,
--   return 'Nothing' when vector is empty.
foldr1Maybe' :: forall v a. Vec v a => (a -> a -> a) -> v a -> Maybe a
{-# INLINE foldr1Maybe' #-}
foldr1Maybe' f (Vec arr s l)
    | l <= 0 = Nothing
    | otherwise = case indexArr' arr (s+l-1) of
                    (# x0 #) -> let !r = foldl' f x0 (fromArr arr s (l-1) :: v a)
                                in Just r

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
    copy (Vec ba s l:vs) !i !marr = do when (l /= 0) (copyArr marr i ba s l)
                                       copy vs (i+l) marr

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

-- | /O(n)/ 'maximum' returns the maximum value from a vector,
--   return 'Nothing' in the case of an empty vector.
maximumMaybe :: (Vec v a, Ord a) => v a -> Maybe a
{-# INLINE maximumMaybe #-}
maximumMaybe = foldl1Maybe' max

-- | /O(n)/ 'minimum' returns the minimum value from a 'vector'
--
-- An 'EmptyVector' exception will be thrown in the case of an empty vector.
minimum :: (Vec v a, Ord a, HasCallStack) => v a -> a
{-# INLINE minimum #-}
minimum = foldl1' min

-- | /O(n)/ 'minimum' returns the minimum value from a vector,
--   return 'Nothing' in the case of an empty vector.
minimumMaybe :: (Vec v a, Ord a) => v a -> Maybe a
{-# INLINE minimumMaybe #-}
minimumMaybe = foldl1Maybe' min

-- | /O(n)/ 'product' returns the product value from a vector
product :: (Vec v a, Num a) => v a -> a
{-# INLINE product #-}
product = foldl' (*) 1

-- | /O(n)/ 'product' returns the product value from a vector
--
-- This function will shortcut on zero. Note this behavior change the semantics
-- for lifted vector: @product [1,0,undefined] /= product' [1,0,undefined]@.
product' :: (Vec v a, Num a, Eq a) => v a -> a
{-# INLINE product' #-}
product' (Vec arr s l) = go 1 s
  where
    !end = s+l
    go !acc !i | acc == 0  = 0
               | i >= end  = acc
               | otherwise = case indexArr' arr i of
                                (# x #) -> go (acc*x) (i+1)

-- | /O(n)/ Applied to a predicate and a vector, 'any' determines
-- if any elements of the vector satisfy the predicate.
any :: Vec v a => (a -> Bool) -> v a -> Bool
{-# INLINE any #-}
any f (Vec arr s l)
    | l <= 0    = False
    | otherwise = case indexArr' arr s of
                    (# x0 #) -> go (f x0) (s+1)
  where
    !end = s+l
    go !acc !i | acc       = True
               | i >= end  = acc
               | otherwise = case indexArr' arr i of
                                (# x #) -> go (acc || f x) (i+1)

-- | /O(n)/ Applied to a predicate and a vector, 'all' determines
-- if all elements of the vector satisfy the predicate.
all :: Vec v a => (a -> Bool) -> v a -> Bool
{-# INLINE all #-}
all f (Vec arr s l)
    | l <= 0    = True
    | otherwise = case indexArr' arr s of
                    (# x0 #) -> go (f x0) (s+1)
  where
    !end = s+l
    go !acc !i | not acc   = False
               | i >= end  = acc
               | otherwise = case indexArr' arr i of
                                (# x #) -> go (acc && f x) (i+1)

-- | /O(n)/ 'sum' returns the sum value from a 'vector'
sum :: (Vec v a, Num a) => v a -> a
{-# INLINE sum #-}
sum = foldl' (+) 0

-- | /O(n)/ 'count' returns count of an element from a 'vector'
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
    go acc !i !marr
        | i >= end = return acc
        | otherwise = do
            x <- indexArrM ba i
            let (acc', c) = acc `f` x
            writeArr marr (i-s) c
            go acc' (i+1) marr

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
    go acc !i !marr
        | i < s     = return acc
        | otherwise = do
            x <- indexArrM ba i
            let (acc', c) = acc `f` x
            writeArr marr (i-s) c
            go acc' (i-1) marr

--------------------------------------------------------------------------------
--  Generating and unfolding vector.
--
-- | /O(n)/ 'replicate' @n x@ is a vector of length @n@ with @x@
-- the value of every element.
--
-- Note: 'replicate' will not force the element in boxed vector case.
replicate :: (Vec v a) => Int -> a -> v a
{-# INLINE replicate #-}
replicate n x | n <= 0    = empty
              | otherwise = create n (\ marr -> setArr marr 0 n x)

-- | /O(n*m)/ 'cycleN' a vector n times.
cycleN :: forall v a. Vec v a => Int -> v a -> v a
{-# INLINE cycleN #-}
cycleN n (Vec arr s l)
    | l == 0    = empty
    | otherwise = create end (go 0)
  where
    !end = n*l
    go :: Int -> MArray v s a -> ST s ()
    go !i !marr | i >= end  = return ()
                | otherwise = copyArr marr i arr s l >> go (i+l) marr

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
    go !acc !i !marr
      | n == i    = return (Just acc, i)
      | otherwise = case f acc of
          Nothing        -> return (Nothing, i)
          Just (x, acc') -> do writeArr marr i x
                               go acc' (i+1) marr

--------------------------------------------------------------------------------
-- Searching by equality

-- | /O(n)/ 'elem' test if given element is in given vector.
elem :: (Vec v a, Eq a) => a -> v a -> Bool
{-# INLINE elem #-}
elem x = isJust . elemIndex x

-- | /O(n)/ 'not . elem'
notElem ::  (Vec v a, Eq a) => a -> v a -> Bool
{-# INLINE notElem #-}
notElem x = not . elem x

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given vector which is equal to the query
-- element, or 'Nothing' if there is no such element.
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

-- | /O(n)/ Special 'elemIndex' for 'Bytes' using @memchr(3)@
--
-- On most platforms @memchr(3)@ is a highly optimized byte searching
-- function, thus we make a special binding for it.
--
-- A rewrite rule @elemIndex = elemIndexBytes@ is also included.
elemIndexBytes :: Word8 -> Bytes -> Maybe Int
{-# INLINE elemIndexBytes #-}
elemIndexBytes w (PrimVector (PrimArray ba#) s l) =
    case fromIntegral (c_memchr ba# s w l) of
        -1 -> Nothing
        r  -> Just r

--------------------------------------------------------------------------------

-- | Index pair type to help GHC unpack in some loops, useful when write fast folds.
data IPair a = IPair { ifst :: {-# UNPACK #-}!Int, isnd :: a } deriving (Show, Eq, Ord)

instance (Arbitrary v) => Arbitrary (IPair v) where
    arbitrary = iPairFromTuple <$> arbitrary
    shrink v = iPairFromTuple <$> shrink (iPairToTuple v)

instance (CoArbitrary v) => CoArbitrary (IPair v) where
    coarbitrary = coarbitrary . iPairToTuple

instance Functor IPair where
    {-# INLINE fmap #-}
    fmap f (IPair i v) = IPair i (f v)

instance NFData a => NFData (IPair a) where
    {-# INLINE rnf #-}
    rnf (IPair _ a) = rnf a

-- | Unlike 'Functor' instance, this mapping evaluate value inside 'IPair' strictly.
mapIPair' :: (a -> b) -> IPair a -> IPair b
{-# INLINE mapIPair' #-}
mapIPair' f (IPair i v) = let !v' = f v in IPair i (f v)

iPairToTuple :: IPair a -> (Int, a)
{-# INLINE iPairToTuple #-}
iPairToTuple (IPair i v) = (i, v)

iPairFromTuple :: (Int, a) -> IPair a
{-# INLINE iPairFromTuple #-}
iPairFromTuple (i, v) = IPair i v

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

-- | Cast between vectors
castVector :: (Vec v a, Cast a b) => v a -> v b
castVector = unsafeCoerce#

--------------------------------------------------------------------------------

foreign import ccall unsafe "string.h strcmp"
    c_strcmp :: Addr# -> Addr# -> IO CInt

foreign import ccall unsafe "string.h strlen"
    c_strlen :: Addr# -> IO CSize

foreign import ccall unsafe "text.h ascii_validate_addr"
    c_ascii_validate_addr :: Addr# -> Int -> IO Int

foreign import ccall unsafe "bytes.h hs_fnv_hash_addr"
    c_fnv_hash_addr :: Addr# -> Int -> Int -> IO Int

foreign import ccall unsafe "bytes.h hs_fnv_hash"
    c_fnv_hash_ba :: ByteArray# -> Int -> Int -> Int -> IO Int
