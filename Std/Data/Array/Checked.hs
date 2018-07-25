{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Std.Data.Array.Checked
Description : Bounded checked boxed and unboxed arrays
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provides exactly the same API with "Data.Array", but will throw an 'IndexOutOfBounds'
'ArrayException' on bound check failure.

-}
module Std.Data.Array.Checked
  ( -- * Arr typeclass re-export
    A.Arr
  , RealWorld
    -- * Bound checked array operations
  , newArr
  , newArrWith
  , readArr
  , writeArr
  , setArr
  , indexArr
  , indexArrM
  , freezeArr
  , thawArr
  , copyArr
  , copyMutableArr
  , moveArr
  , cloneArr
  , cloneMutableArr
  , resizeMutableArr
  , shrinkMutableArr
  -- * No bound checked operations
  , A.unsafeFreezeArr
  , A.unsafeThawArr
  , A.sameMutableArr
  , A.sizeofArr
  , A.sizeofMutableArr
  , A.sameArr
  -- * Boxed array type
  , A.Array(..)
  , A.MutableArray(..)
  , A.SmallArray(..)
  , A.SmallMutableArray(..)
  , A.uninitialized
  -- * Primitive array type
  , A.PrimArray(..)
  , A.MutablePrimArray(..)
  -- * Bound checked primitive array operations
  , newPinnedPrimArray, newAlignedPinnedPrimArray
  , copyPrimArrayToPtr, copyMutablePrimArrayToPtr, copyPtrToMutablePrimArray
  -- * No bound checked primitive array operations
  , A.primArrayContents, A.mutablePrimArrayContents, A.withPrimArrayContents, A.withMutablePrimArrayContents
  , A.isPrimArrayPinned, A.isMutablePrimArrayPinned
  -- * Unlifted array type
  , A.UnliftedArray(..)
  , A.MutableUnliftedArray(..)
  , A.PrimUnlifted(..)
  -- * The 'ArrayException' type
  , ArrayException(..)
  ) where

import qualified Std.Data.Array as A
import Control.Exception (throw, ArrayException(..))
import GHC.Stack
import Data.Typeable
import Control.Monad.Primitive
import Data.Primitive.Types
import GHC.Ptr (Ptr(..))

check :: HasCallStack => Bool -> a -> a
{-# INLINE check #-}
check True  x = x
check False _ = throw (IndexOutOfBounds $ show callStack)

newArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
       => Int -> m (marr s a)
newArr n = check  (n>=0) (A.newArr n)
{-# INLINE newArr #-}

newArrWith :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
           => Int -> a -> m (marr s a)
newArrWith n x = check  (n>=0) (A.newArrWith n x)
{-# INLINE newArrWith #-}

readArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
        => marr s a -> Int -> m a
readArr marr i = do
    siz <- A.sizeofMutableArr marr
    check
        (i>=0 && i<siz)
        (A.readArr marr i)
{-# INLINE readArr #-}

writeArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
         => marr s a -> Int -> a -> m ()
writeArr marr i x = do
    siz <- A.sizeofMutableArr marr
    check
        (i>=0 && i<siz)
        (A.writeArr marr i x)
{-# INLINE writeArr #-}

setArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
       => marr s a -> Int -> Int -> a -> m ()
setArr marr s l x = do
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.setArr marr s l x)
{-# INLINE setArr #-}

indexArr :: (A.Arr marr arr a, HasCallStack)
         => arr a -> Int -> a
indexArr arr i = check
    (i>=0 && i<A.sizeofArr arr)
    (A.indexArr arr i)
{-# INLINE indexArr #-}

indexArrM :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
          => arr a -> Int -> m a
indexArrM arr i = check
    (i>=0 && i<A.sizeofArr arr)
    (A.indexArrM arr i)
{-# INLINE indexArrM #-}

freezeArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
          => marr s a -> Int -> Int -> m (arr a)
freezeArr marr s l = do
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.freezeArr marr s l)
{-# INLINE freezeArr #-}

thawArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
        => arr a -> Int -> Int -> m (marr s a)
thawArr arr s l = check
    (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
    (A.thawArr arr s l)
{-# INLINE thawArr #-}

copyArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
        => marr s a -> Int -> arr a -> Int -> Int -> m ()
copyArr marr s1 arr s2 l = do
    siz <- A.sizeofMutableArr marr
    check
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=A.sizeofArr arr && (s1+l)<=siz)
        (A.copyArr marr s1 arr s2 l)
{-# INLINE copyArr #-}

copyMutableArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
               => marr s a -> Int -> marr s a -> Int -> Int -> m ()
copyMutableArr marr1 s1 marr2 s2 l = do
    siz1 <- A.sizeofMutableArr marr1
    siz2 <- A.sizeofMutableArr marr2
    check
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
        (A.copyMutableArr marr1 s1 marr2 s2 l)
{-# INLINE copyMutableArr #-}

moveArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
        => marr s a -> Int -> marr s a -> Int -> Int -> m ()
moveArr marr1 s1 marr2 s2 l = do
    siz1 <- A.sizeofMutableArr marr1
    siz2 <- A.sizeofMutableArr marr2
    check
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
        (A.copyMutableArr marr1 s1 marr2 s2 l)
{-# INLINE moveArr #-}

cloneArr :: (A.Arr marr arr a, HasCallStack)
         => arr a -> Int -> Int -> arr a
cloneArr arr s l = check
    (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
    (A.cloneArr arr s l)
{-# INLINE cloneArr #-}

cloneMutableArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
                => marr s a -> Int -> Int -> m (marr s a)
cloneMutableArr marr s l = do
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.cloneMutableArr marr s l)
{-# INLINE cloneMutableArr #-}

resizeMutableArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
                 => marr s a -> Int -> m (marr s a)
resizeMutableArr marr n = check
    (n>=0)
    (A.resizeMutableArr marr n)
{-# INLINE resizeMutableArr #-}

-- | New size should be >= 0, and <= original size.
--
shrinkMutableArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
                 => marr s a -> Int -> m ()
shrinkMutableArr marr n = do
    siz <- A.sizeofMutableArr marr
    check
        (n>=0 && n<=siz)
        (A.shrinkMutableArr marr n)
{-# INLINE shrinkMutableArr #-}

--------------------------------------------------------------------------------

-- | Create a /pinned/ byte array of the specified size,
-- The garbage collector is guaranteed not to move it.
newPinnedPrimArray :: (PrimMonad m, Prim a, HasCallStack)
                   => Int -> m (A.MutablePrimArray (PrimState m) a)
{-# INLINE newPinnedPrimArray #-}
newPinnedPrimArray n =
    check  (n>=0) (A.newPinnedPrimArray n)

-- | Create a /pinned/ primitive array of the specified size and respect given primitive type's
-- alignment. The garbage collector is guaranteed not to move it.
--
newAlignedPinnedPrimArray :: (PrimMonad m, Prim a, HasCallStack)
                          => Int -> m (A.MutablePrimArray (PrimState m) a)
{-# INLINE newAlignedPinnedPrimArray #-}
newAlignedPinnedPrimArray n =
    check  (n>=0) (A.newAlignedPinnedPrimArray n)

copyPrimArrayToPtr :: (PrimMonad m, Prim a, HasCallStack)
                   => Ptr a
                   -> A.PrimArray a
                   -> Int
                   -> Int
                   -> m ()
{-# INLINE copyPrimArrayToPtr #-}
copyPrimArrayToPtr ptr arr s l = check
    (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
    (A.copyPrimArrayToPtr ptr arr s l)

copyMutablePrimArrayToPtr :: (PrimMonad m, Prim a, HasCallStack)
                          => Ptr a
                          -> A.MutablePrimArray (PrimState m) a
                          -> Int
                          -> Int
                          -> m ()
{-# INLINE copyMutablePrimArrayToPtr #-}
copyMutablePrimArrayToPtr ptr marr s l = do
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.copyMutablePrimArrayToPtr ptr marr s l)

copyPtrToMutablePrimArray :: (PrimMonad m, Prim a, HasCallStack)
                            => A.MutablePrimArray (PrimState m) a
                            -> Int
                            -> Ptr a
                            -> Int
                            -> m ()
{-# INLINE copyPtrToMutablePrimArray #-}
copyPtrToMutablePrimArray marr s ptr l = do
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.copyPtrToMutablePrimArray marr s ptr l)
