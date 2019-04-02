{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Std.Foreign.PrimArray
Description : Use PrimArray with FFI
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
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
    The main use case for @safe@ FFIs are long running functions, for example, doing IO polling.
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
will result in segfault.

For convention you should always use `Ptr a` as the tagged pointer type, and `Addr` as the raw address type, use `addrToPtr/ptrToAddr` to cast between them if needed.

-}

module Std.Foreign.PrimArray
  ( -- ** Unsafe FFI
    withPrimArrayUnsafe
  , withMutablePrimArrayUnsafe
  , withMutableByteArrayUnsafe
  , withPrimVectorUnsafe
  , withPrimUnsafe
  , withPrimUnsafe'
    -- ** Safe FFI
  , withPrimArraySafe
  , withMutablePrimArraySafe
  , withMutableByteArraySafe
  , withPrimVectorSafe
  , withPrimSafe
  , withPrimSafe'
  , bytesToByteString
  , bytesFromByteString
    -- ** Pointer helpers
  , BA#, MBA#
  , clearPtr
  , addrToPtr
  , ptrToAddr
  , castPtr
  -- ** re-export
  , module GHC.Prim
  , module Data.Primitive.Ptr
  ) where

import           Control.Monad.Primitive
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Unsafe   as BS
import           Data.Primitive
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           Data.Primitive.Ptr
import           Data.Word
import           Foreign.C.Types
import           GHC.Prim
import           GHC.Ptr
import           Std.Data.Array
import           Std.Data.Vector.Base
import           Std.IO.Exception
import           Std.IO.Resource

-- | Type alias for 'ByteArray#'.
--
-- Since we can't newtype an unlifted type yet, type alias is the best we can get
-- to describe a 'ByteArray#' which we are going to pass across FFI. At C side you
-- should use a proper const pointer type.
--
-- Don't cast 'BA#' to 'Addr#' since the heap object offset is hard-coded in code generator:
-- <https://github.com/ghc/ghc/blob/master/compiler/codeGen/StgCmmForeign.hs#L520>
--
-- USE THIS TYPE WITH UNSAFE FFI CALL ONLY.
type BA# a = ByteArray#

-- | Type alias for 'MutableByteArray#' 'RealWorld'.
--
-- Since we can't newtype an unlifted type yet, type alias is the best we can get
-- to describe a 'MutableByteArray#' which we are going to pass across FFI. At C side you
-- should use a proper pointer type.
--
-- Don't cast 'MBA#' to 'Addr#' since the heap object offset is hard-coded in code generator:
-- <https://github.com/ghc/ghc/blob/master/compiler/codeGen/StgCmmForeign.hs#L520>
--
-- USE THIS TYPE WITH UNSAFE FFI CALL ONLY.
type MBA# a = MutableByteArray# RealWorld

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
-- So it's users' responsibility to make sure the array content is not mutated (a const pointer type may help).
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withPrimArrayUnsafe :: (Prim a) => PrimArray a -> (BA# a -> Int -> IO b) -> IO b
withPrimArrayUnsafe pa@(PrimArray ba#) f = f ba# (sizeofPrimArray pa)

-- | Pass mutable primitive array to unsafe FFI as pointer.
--
-- The mutable version of 'withPrimArrayUnsafe'.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withMutablePrimArrayUnsafe :: (Prim a) => MutablePrimArray RealWorld a
                           -> (MBA# a -> Int -> IO b) -> IO b
withMutablePrimArrayUnsafe mpa@(MutablePrimArray mba#) f =
    getSizeofMutablePrimArray mpa >>= f mba#

withMutableByteArrayUnsafe :: Int      -- ^ In bytes
                           -> (MBA# Word8 -> IO b) -> IO b
withMutableByteArrayUnsafe len f = do
    (MutableByteArray mba#) <- newByteArray len
    f mba#

-- | Pass 'PrimVector' to unsafe FFI as pointer
--
-- The 'PrimVector' version of 'withPrimArrayUnsafe'.
--
-- The second 'Int' arguement is the first element offset, the third 'Int' argument is the
-- element length.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withPrimVectorUnsafe :: (Prim a)
                     => PrimVector a -> (BA# a -> Int -> Int -> IO b) -> IO b
withPrimVectorUnsafe (PrimVector arr s l) f = withPrimArrayUnsafe arr $ \ ba# _ -> f ba# s l


-- | Create an one element primitive array and use it as a pointer to the primitive element.
--
-- Return the element and the computation result.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withPrimUnsafe :: (Prim a)
               => a -> (MBA# a -> IO b) -> IO (a, b)
withPrimUnsafe v f = do
    mpa@(MutablePrimArray mba#) <- newPrimArray 1    -- All heap objects are WORD aligned
    writePrimArray mpa 0 v
    !b <- f mba#                                      -- so no need to do extra alignment
    !a <- readPrimArray mpa 0
    return (a, b)

withPrimUnsafe' :: (Prim a)
               => (MBA# a -> IO b) -> IO (a, b)
withPrimUnsafe' f = do
    mpa@(MutablePrimArray mba#) <- newPrimArray 1    -- All heap objects are WORD aligned
    !b <- f mba#                                      -- so no need to do extra alignment
    !a <- readPrimArray mpa 0
    return (a, b)

--------------------------------------------------------------------------------

-- | Pass primitive array to safe FFI as pointer.
--
-- Use proper pointer type and @CSize/CSsize@ to marshall @Ptr a@ and @Int@ arguments on C side.
-- The memory pointed by 'Ptr a' will not moved.
--
-- The second 'Int' arguement is the element size not the bytes size.
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimArraySafe :: (Prim a) => PrimArray a -> (Ptr a -> Int -> IO b) -> IO b
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
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withMutablePrimArraySafe :: (Prim a) => MutablePrimArray RealWorld a -> (Ptr a -> Int -> IO b) -> IO b
withMutablePrimArraySafe marr f
    | isMutablePrimArrayPinned marr = do
        siz <- getSizeofMutablePrimArray marr
        withMutablePrimArrayContents marr $ \ ptr -> f ptr siz
    | otherwise = do
        siz <- getSizeofMutablePrimArray marr
        buf <- newPinnedPrimArray siz
        copyMutablePrimArray buf 0 marr 0 siz
        withMutablePrimArrayContents buf $ \ ptr -> f ptr siz

withMutableByteArraySafe :: Int -> (Ptr Word8 -> IO b) -> IO b
withMutableByteArraySafe siz f = do
    buf <- newPinnedPrimArray siz
    withMutablePrimArrayContents buf f

-- | Pass 'PrimVector' to unsafe FFI as pointer
--
-- The 'PrimVector' version of 'withPrimArraySafe'. The 'Ptr' is already pointed
-- to the first element, thus no offset is provided.
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimVectorSafe :: forall a b. (Prim a) => PrimVector a -> (Ptr a -> Int -> IO b) -> IO b
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
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimSafe :: forall a b. Prim a => a -> (Ptr a -> IO b) -> IO (a, b)
withPrimSafe v f = do
    buf <- newAlignedPinnedPrimArray 1
    writePrimArray buf 0 v
    !b <- withMutablePrimArrayContents buf $ \ ptr -> f ptr
    !a <- readPrimArray buf 0
    return (a, b)

withPrimSafe' :: forall a b. Prim a => (Ptr a -> IO b) -> IO (a, b)
withPrimSafe' f = do
    buf <- newAlignedPinnedPrimArray 1
    !b <- withMutablePrimArrayContents buf $ \ ptr -> f ptr
    !a <- readPrimArray buf 0
    return (a, b)

-- | /O(n)/ Copy the contents of a Vector into a new ByteString
bytesToByteString :: Bytes -> IO ByteString
{-# INLINE bytesToByteString #-}
bytesToByteString bytes =
    withPrimVectorSafe bytes (\ptr len -> BS.packCStringLen (castPtr ptr, len))

-- | /O(n)/ Copy the contents of a ByteString into a new Vector
bytesFromByteString :: ByteString -> IO Bytes
{-# INLINE bytesFromByteString #-}
bytesFromByteString bs = BS.unsafeUseAsCString bs $ \ptr ->
    pure $ create len (copy ptr)
  where
    len = BS.length bs
    copy ptr mpa = do
        copyPtrToMutablePrimArray mpa 0 (castPtr ptr) len
        writeArr mpa len 0

--------------------------------------------------------------------------------

foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()

-- | Zero a structure.
--
-- There's no 'Storable' or 'Prim' constraint on 'a' type, thus the length
-- should be given in bytes.
--
clearPtr :: Ptr a -> Int -> IO ()
clearPtr dest nbytes = memset dest 0 (fromIntegral nbytes)

-- | Cast between raw address and tagged pointer.
addrToPtr :: Addr -> Ptr a
addrToPtr (Addr addr#) = Ptr addr#

-- | Cast between tagged pointer and raw address.
ptrToAddr :: Ptr a -> Addr
ptrToAddr (Ptr addr#) = Addr addr#
