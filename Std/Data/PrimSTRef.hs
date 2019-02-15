{-|
Module      :  Std.Data.PrimSTRef
Copyright   :  (c) Dong Han 2017~2019
License     :  BSD-style

Maintainer  :  winterland1989@gmail.com
Stability   :  experimental
Portability :  portable

This module provide fast unboxed references for ST monad. Unboxed reference is implemented using single cell MutableByteArray s to eliminate indirection overhead which MutVar# s a carry, on the otherhand unboxed reference only support limited type(instances of Prim class).
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
