{-# LANGUAGE MagicHash, UnboxedTuples #-}

{-|
Module      :  Std.Data.PrimIORef
Copyright   :  (c) Dong Han 2017~2019
License     :  BSD-style

Maintainer  :  winterland1989@gmail.com
Stability   :  experimental
Portability :  portable

This package provide fast unboxed references for IO monad and atomic operations for 'Counter' type. Unboxed reference is implemented using single cell MutableByteArray s to eliminate indirection overhead which MutVar# s a carry, on the otherhand unboxed reference only support limited type(instances of Prim class).

A simple diagram could show the difference between IORef Int with PrimIORef Int:

data Foo = Foo {-# UNPACK #-} (IORef Int)

        +-----------+    +-------------+    +---------+
        | Foo |  *  +--->+ MutVar# | * +--->+ I# | i# |
        +-----------+    +-------------+    +---------+

data Bar = Bar {-# UNPACK #-} (PrimIORef Int)

        +-----------+    +------------------------+
        | Bar |  *  +--->+ MutableByteArray# | i# |
        +-----------+    +------------------------+

Atomic operations on 'Counter' type are implemented using fetch-and-add primitives, which is much faster than a CAS loop(@atomicModifyIORef@). Beside basic atomic counter usage, you can also leverage idempotence of @and 0@, @or (-1)@ to make a concurrent flag.
-}



module Std.Data.PrimIORef
  ( -- * Unboxed IO references
    PrimIORef
  , newPrimIORef
  , readPrimIORef
  , writePrimIORef
  , modifyPrimIORef
    -- * Atomic operations for @PrimIORef Int@
  , Counter
  , newCounter
    -- ** return value BEFORE atomic operation
  , atomicAddCounter
  , atomicSubCounter
  , atomicAndCounter
  , atomicNandCounter
  , atomicOrCounter
  , atomicXorCounter
    -- ** return value AFTER atomic operation
  , atomicAddCounter'
  , atomicSubCounter'
  , atomicAndCounter'
  , atomicNandCounter'
  , atomicOrCounter'
  , atomicXorCounter'
    -- ** without returning
  , atomicAddCounter_
  , atomicSubCounter_
  , atomicAndCounter_
  , atomicNandCounter_
  , atomicOrCounter_
  , atomicXorCounter_
  ) where

import Data.Primitive.Types
import Data.Primitive.ByteArray
import GHC.Prim
import GHC.Types
import GHC.ST
import GHC.IO(stToIO)
import Std.Data.PrimSTRef.Base

-- | A mutable variable in the IO monad which can hold an instance of 'Prim'.
newtype PrimIORef a = PrimIORef (PrimSTRef RealWorld a)

-- | Build a new 'PrimIORef'
newPrimIORef :: Prim a => a -> IO (PrimIORef a)
newPrimIORef init = PrimIORef `fmap` stToIO (newPrimSTRef init)
{-# INLINE newPrimIORef #-}

-- | Read the value of an 'PrimIORef'
readPrimIORef :: Prim a => PrimIORef a -> IO a
readPrimIORef (PrimIORef ref) = stToIO (readPrimSTRef ref)
{-# INLINE readPrimIORef #-}

-- | Write a new value into an 'PrimIORef'
writePrimIORef :: Prim a => PrimIORef a -> a -> IO ()
writePrimIORef (PrimIORef ref) x = stToIO (writePrimSTRef ref x)
{-# INLINE writePrimIORef #-}

-- | Mutate the contents of an 'IORef'.
--
--  Unboxed reference is always strict on the value it hold.
modifyPrimIORef :: Prim a => PrimIORef a -> (a -> a) -> IO ()
modifyPrimIORef ref f = readPrimIORef ref >>= writePrimIORef ref . f
{-# INLINE modifyPrimIORef #-}

-- | Alias for 'PrimIORef Int' which support several atomic operations.
type Counter = PrimIORef Int

-- | Build a new 'Counter'
newCounter :: Int -> IO Counter
newCounter = newPrimIORef
{-# INLINE newCounter #-}

-- | Atomically add a 'Counter', return the value AFTER added.
atomicAddCounter' :: Counter -> Int -> IO Int
atomicAddCounter' (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchAddIntArray# mba# 0# x# s1# in (# s2#, (I# (res# +# x#)) #)

-- | Atomically add a 'Counter', return the value BEFORE added.
atomicAddCounter :: Counter -> Int -> IO Int
atomicAddCounter (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchAddIntArray# mba# 0# x# s1# in (# s2#, (I# res#) #)

-- | Atomically add a 'Counter'.
atomicAddCounter_ :: Counter -> Int -> IO ()
atomicAddCounter_ (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, _ #) = fetchAddIntArray# mba# 0# x# s1# in (# s2#, () #)
{-# INLINE atomicAddCounter_ #-}


-- | Atomically sub a 'Counter', return the value AFTER subbed.
atomicSubCounter' :: Counter -> Int -> IO Int
atomicSubCounter' (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchSubIntArray# mba# 0# x# s1# in (# s2#, (I# (res# -# x#)) #)

-- | Atomically sub a 'Counter', return the value BEFORE subbed.
atomicSubCounter :: Counter -> Int -> IO Int
atomicSubCounter (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchSubIntArray# mba# 0# x# s1# in (# s2#, (I# res#) #)

-- | Atomically sub a 'Counter'
atomicSubCounter_ :: Counter -> Int -> IO ()
atomicSubCounter_ (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, _ #) = fetchSubIntArray# mba# 0# x# s1# in (# s2#, () #)
{-# INLINE atomicSubCounter_ #-}

-- | Atomically and a 'Counter', return the value AFTER anded.
atomicAndCounter' :: Counter -> Int -> IO Int
atomicAndCounter' (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchAndIntArray# mba# 0# x# s1# in (# s2#, (I# (res# `andI#` x#)) #)
{-# INLINE atomicAndCounter' #-}

-- | Atomically and a 'Counter', return the value BEFORE anded.
atomicAndCounter :: Counter -> Int -> IO Int
atomicAndCounter (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchAndIntArray# mba# 0# x# s1# in (# s2#, (I# res#) #)
{-# INLINE atomicAndCounter #-}

-- | Atomically and a 'Counter'
atomicAndCounter_ :: Counter -> Int -> IO ()
atomicAndCounter_ (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchAndIntArray# mba# 0# x# s1# in (# s2#, () #)
{-# INLINE atomicAndCounter_ #-}

-- | Atomically nand a 'Counter', return the value AFTER nanded.
atomicNandCounter' :: Counter -> Int -> IO Int
atomicNandCounter' (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchNandIntArray# mba# 0# x# s1# in (# s2#, (I# (notI# (res# `andI#` x#))) #)
{-# INLINE atomicNandCounter' #-}

-- | Atomically nand a 'Counter', return the value BEFORE nanded.
atomicNandCounter :: Counter -> Int -> IO Int
atomicNandCounter (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchNandIntArray# mba# 0# x# s1# in (# s2#, (I# res#) #)
{-# INLINE atomicNandCounter #-}

-- | Atomically nand a 'Counter'
atomicNandCounter_ :: Counter -> Int -> IO ()
atomicNandCounter_ (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchNandIntArray# mba# 0# x# s1# in (# s2#, () #)
{-# INLINE atomicNandCounter_ #-}

-- | Atomically or a 'Counter', return the value AFTER ored.
atomicOrCounter' :: Counter -> Int -> IO Int
atomicOrCounter' (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchOrIntArray# mba# 0# x# s1# in (# s2#, (I# (res# `orI#` x#)) #)
{-# INLINE atomicOrCounter' #-}

-- | Atomically or a 'Counter', return the value BEFORE ored.
atomicOrCounter :: Counter -> Int -> IO Int
atomicOrCounter (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchOrIntArray# mba# 0# x# s1# in (# s2#, (I# res#) #)
{-# INLINE atomicOrCounter #-}

-- | Atomically or a 'Counter'
atomicOrCounter_ :: Counter -> Int -> IO ()
atomicOrCounter_ (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchOrIntArray# mba# 0# x# s1# in (# s2#, () #)
{-# INLINE atomicOrCounter_ #-}

-- | Atomically xor a 'Counter', return the value AFTER xored.
atomicXorCounter' :: Counter -> Int -> IO Int
atomicXorCounter' (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchXorIntArray# mba# 0# x# s1# in (# s2#, (I# (res# `xorI#` x#)) #)
{-# INLINE atomicXorCounter' #-}

-- | Atomically xor a 'Counter', return the value BEFORE xored.
atomicXorCounter :: Counter -> Int -> IO Int
atomicXorCounter (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchXorIntArray# mba# 0# x# s1# in (# s2#, (I# res#) #)
{-# INLINE atomicXorCounter #-}

-- | Atomically xor a 'Counter'
atomicXorCounter_ :: Counter -> Int -> IO ()
atomicXorCounter_ (PrimIORef (PrimSTRef (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res# #) = fetchXorIntArray# mba# 0# x# s1# in (# s2#, () #)
{-# INLINE atomicXorCounter_ #-}
