{-|
Module      :  Std.Data.PrimRef
Copyright   :  (c) Dong Han 2017~2019
License     :  BSD-style

Maintainer  :  winterland1989@gmail.com
Stability   :  experimental
Portability :  portable

This module provide fast unboxed references for ST monad. Unboxed reference is implemented using single cell MutableByteArray s to eliminate indirection overhead which MutVar# s a carry, on the otherhand unboxed reference only support limited type(instances of Prim class).

A simple diagram could show the difference between STRef Int with PrimSTRef Int:

data Foo = Foo {-# UNPACK #-} (STRef Int)

        +-----------+    +-------------+    +---------+
        | Foo |  *  +--->+ MutVar# | * +--->+ I# | i# |
        +-----------+    +-------------+    +---------+

data Bar = Bar {-# UNPACK #-} (PrimSTRef Int)

        +-----------+    +------------------------+
        | Bar |  *  +--->+ MutableByteArray# | i# |
        +-----------+    +------------------------+
-}

module Std.Data.PrimSTRef
  ( -- * Unboxed ST references
    PrimSTRef
  , newPrimSTRef
  , readPrimSTRef
  , writePrimSTRef
  , modifyPrimSTRef
  ) where

import Std.Data.PrimSTRef.Base
