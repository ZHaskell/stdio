{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Foreign.PrimArray
Description : Use PrimArray with FFI
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide functions for using 'PrimArray' and 'PrimVector' with GHC FFI(Foreign function interface).
Since GHC runtime is garbaged collected, we have a quite complex story when passing primitive arrays to FFI.
We have two types of primitive array in GHC, with the objective to minimize overall memory management cost:

  * Small primitive arrays created with 'newPrimArray' are directly allocated on GHC heap, which can be moved
    by GHC garbage collector, we call these arrays @unpinned@. Allocating these array is cheap, we only need
    to check heap limit and bump heap pointer just like any other haskell heap objects. But we will pay GC cost
    , which is OK for small arrays.

  * Large primitive array and those created with 'newPinnedPrimArray' are allocated on GHC managed memory blocks,
    which is also traced by garbage collector, but will never moved before freed, thus are called @pinned@.
    Allocating these arrays are bit more expensive since it's more like how @malloc@ works, but we don't have to
    pay for GC cost.

Beside the @pinned/unpinned@ difference, we also have two types of FFI calls in GHC:

  * Safe FFI call annotated with @safe@ keyword. These calls are executed on separated OS thread, which can be
    running concurrently with GHC garbage collector, thus we want to make sure only pinned arrays are passed.
    The main use case for @safe@ FFIs are long running functions, for example, doing I/O polling.
    Since these calls are running on separated OS thread, haskell thread on original OS thread will not be affected.

  * Unsafe FFI call annotated with @unsafe@ keyword. These calls are executed on the same OS thread which is
    running the haskell side FFI code, which will in turn stop GHC from doing a garbage collection. We can pass
    both 'pinned' and 'unpinned' arrays in this case. The use case for @unsafe@ FFIs are short/small functions,
    which can be treated like a fat primitive operations, such as @memcpy@, @memcmp@. Using @unsafe@ FFI with
    long running functions will effectively block GHC runtime thread from running any other haskell thread, which
    is dangerous. Even if you use threaded runtime and expect your haskell thread can be stolen by other OS thread,
    but this will not work since GHC garbage collector will refuse to run if one of the OS thread is blocked by
    FFI calls.

Base on above analysis, we have following FFI strategy table.

  +--------------+---------------+---------------+
  | FFI  \ Array |    pinned     |   unpinned    |
  +--------------+---------------+---------------+
  |   unsafe     | directly pass | directly pass |
  +--------------+---------------+---------------+
  |     safe     | directly pass |  make a copy  |
  +--------------+---------------+---------------+

In this module, we separate safe and unsafe FFI handling due to the strategy difference: if the user can guarantee
the FFI are unsafe, we can save an extra copy and pinned allocation. Mistakenly using unsafe function with safe FFI
will result in severe memory issue.

-}

module Foreign.PrimArray
  ( -- ** Unsafe FFI
    withPrimArrayUnsafe
  , withMutablePrimArrayUnsafe
  , withPrimVectorUnsafe
  , withPrimUnsafe
    -- ** Safe FFI
  , withPrimArraySafe
  , withMutablePrimArraySafe
  , withPrimVectorSafe
  , withPrimSafe
  -- ** re-export
  , module GHC.Prim
  ) where

import GHC.Prim
import Data.Primitive
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Data.Primitive.ByteArray
import Data.Vector
import GHC.Ptr

-- | Pass primitive array to unsafe FFI as pointer.
--
-- Enable 'UnliftedFFITypes' extension in your haskell code, use proper pointer type and @CSize/CSsize@
-- to marshall @ByteArray#@ and @Int@ arguments on C side.
--
-- The second 'Int' arguement is the element size not the bytes size.
--
-- Don't cast 'ByteArray#' to 'Addr#' since the heap object offset is hard-coded in code generator:
-- <https://github.com/ghc/ghc/blob/master/compiler/codeGen/StgCmmForeign.hs#L520>
--
-- In haskell side we use type system to distinguish immutable / mutable arrays, but in C side we can't.
-- So it's users' responsibility to make sure the array content is not mutated. Otherwise please thaw
-- the array and use 'withMutablePrimArrayUnsafe' instead.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withPrimArrayUnsafe :: forall m a b. (PrimMonad m, Prim a)
                    => PrimArray a -> (ByteArray# -> Int -> m b) -> m b
withPrimArrayUnsafe pa@(PrimArray (ByteArray ba#)) f = f ba# (sizeofPrimArray pa)

-- | Pass mutable primitive array to unsafe FFI as pointer.
--
-- The mutable version of 'withPrimArrayUnsafe'.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withMutablePrimArrayUnsafe :: forall m a b. (PrimMonad m, Prim a)
                           => MutablePrimArray (PrimState m) a
                           -> (MutableByteArray# (PrimState m)-> Int -> m b) -> m b
withMutablePrimArrayUnsafe mpa@(MutablePrimArray (MutableByteArray mba#)) f =
    sizeofMutablePrimArray mpa >>= f mba#

-- | Pass 'PrimVector' to unsafe FFI as pointer
--
-- The 'PrimVector' version of 'withPrimArrayUnsafe'.
--
-- The second 'Int' arguement is the first element offset, the third 'Int' argument is the
-- element length.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withPrimVectorUnsafe :: forall m a b. (PrimMonad m, Prim a)
                     => PrimVector a -> (ByteArray# -> Int -> Int -> m b) -> m b
withPrimVectorUnsafe (PrimVector arr s l) f = withPrimArrayUnsafe arr $ \ ba# _ -> f ba# s l


-- | Create an one element primitive array and use it as a pointer to the primitive element.
--
withPrimUnsafe :: forall m a b. (PrimMonad m, Prim a) => (MutableByteArray# (PrimState m) -> m a) -> m a
withPrimUnsafe f = do
    mpa@(MutablePrimArray (MutableByteArray mba#)) <- newPrimArray 1    -- All heap objects are WORD aligned
    f mba#                                                              -- so no need to do extra alignment
    readPrimArray mpa 0

-- | Pass primitive array to safe FFI as pointer.
--
-- Use proper pointer type and @CSize/CSsize@ to marshall @Ptr a@ and @Int@ arguments on C side.
-- The memory pointed by 'Ptr a' will not moved.
--
-- The second 'Int' arguement is the element size not the bytes size.
--
--
withPrimArraySafe :: (PrimMonad m, Prim a) => PrimArray a -> (Ptr a -> Int -> m b) -> m b
withPrimArraySafe arr f
    | isPrimArrayPinned arr = do
        let siz = sizeofPrimArray arr
        withPrimArrayContents arr $ \ ptr -> f ptr siz
    | otherwise = do
        let siz = sizeofPrimArray arr
        buf <- newPinnedPrimArray siz
        copyPrimArray buf 0 arr 0 siz
        withMutablePrimArrayContents buf $ \ ptr -> f ptr siz


-- | Pass mutable primitive array to unsafe FFI as pointer.
--
-- The mutable version of 'withPrimArraySafe'.
--
withMutablePrimArraySafe :: (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> (Ptr a -> Int -> m b) -> m b
withMutablePrimArraySafe marr f
    | isMutablePrimArrayPinned marr = do
        siz <- sizeofMutablePrimArray marr
        withMutablePrimArrayContents marr $ \ ptr -> f ptr siz
    | otherwise = do
        siz <- sizeofMutablePrimArray marr
        buf <- newPinnedPrimArray siz
        copyMutablePrimArray buf 0 marr 0 siz
        withMutablePrimArrayContents buf $ \ ptr -> f ptr siz

-- | Pass 'PrimVector' to unsafe FFI as pointer
--
-- The 'PrimVector' version of 'withPrimArraySafe'. The 'Ptr' is already pointed
-- to the first element, thus no offset is provided.
--
withPrimVectorSafe :: forall m a b. (PrimMonad m, Prim a) => PrimVector a -> (Ptr a -> Int -> m b) -> m b
withPrimVectorSafe v@(PrimVector arr s l) f
    | isPrimArrayPinned arr =
        withPrimArrayContents arr $ \ ptr ->
            let ptr' = ptr `plusPtr` (s * siz) in f ptr' l
    | otherwise = do
        buf <- newPinnedPrimArray l
        copyPrimArray buf 0 arr s l
        withMutablePrimArrayContents buf $ \ ptr -> f ptr l
  where
    siz = sizeOf (undefined :: a)

-- | Create an one element primitive array and use it as a pointer to the primitive element.
--
withPrimSafe :: forall m a b. (PrimMonad m, Prim a) => (MutableByteArray# (PrimState m) -> m a) -> m a
withPrimSafe f = do
    mpa@(MutablePrimArray (MutableByteArray mba#)) <- newAlignedPinnedPrimArray 1
    f mba#
    readPrimArray mpa 0
