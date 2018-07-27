{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Std.Data.PrimArray.BitTwiddle
Description : Primitive bits twiddling
Copyright   : Haskell Foundation, (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module is borrowed from basement's Cast module. The purpose of 'Cast' is to provide primitive types which share the same byte size, so that arrays and vectors parameterized by them can be safely coerced without breaking the index bounds. You can also use it to directly cast primitives just like @reinterpret_cast@.

NOTE, there's some conditional instances base on compiling machine's word size:

@
    instance Cast Word   Word64
    instance Cast Word64 Word
    instance Cast Word   Int64
    instance Cast Int64  Word
    instance Cast Int    Int64
    instance Cast Int64  Int
    instance Cast Int    Word64
    instance Cast Word64 Int
@

are only available on 64bit machines, while

@
    instance Cast Word   Word32
    instance Cast Word32 Word
    instance Cast Word   Int32
    instance Cast Int32  Word
    instance Cast Int    Int32
    instance Cast Int32  Int
    instance Cast Int    Word32
    instance Cast Word32 Int
@

are only available on 32bit machines. Use them with CPP guarded. If possible, avoid use them at all. A 'Coercible' based instance is also provide for convenience.

-}

module Std.Data.PrimArray.Cast
    ( Cast(..)
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.Int
import           GHC.Word
import           GHC.IntWord64
import           GHC.Float
import           Data.Coerce

#include "MachDeps.h"

-- | `Cast` between primitive types of the same size.
--
class Cast source destination where
    cast :: source -> destination

instance {-# OVERLAPPABLE #-} Coercible a b => Cast a b where
    cast = coerce

instance Cast Int8  Word8 where
    cast (I8# i) = W8# (narrow8Word# (int2Word# i))
instance Cast Int16 Word16 where
    cast (I16# i) = W16# (narrow16Word# (int2Word# i))
instance Cast Int32 Word32 where
    cast (I32# i) = W32# (narrow32Word# (int2Word# i))
instance Cast Int64 Word64 where
#if WORD_SIZE_IN_BITS < 64
    cast (I64# i) = W64# (int64ToWord64# i)
#else
    cast (I64# i) = W64# (int2Word# i)
#endif
instance Cast Int   Word where
    cast (I# i) = W# (int2Word# i)

instance Cast Word8  Int8 where
    cast (W8# i) = I8# (narrow8Int# (word2Int# i))
instance Cast Word16 Int16 where
    cast (W16# i) = I16# (narrow16Int# (word2Int# i))
instance Cast Word32 Int32 where
    cast (W32# i) = I32# (narrow32Int# (word2Int# i))
instance Cast Word64 Int64 where
#if WORD_SIZE_IN_BITS < 64
    cast (W64# i) = I64# (word64ToInt64# i)
#else
    cast (W64# i) = I64# (word2Int# i)
#endif
instance Cast Word   Int where
    cast (W# w) = I# (word2Int# w)

#if WORD_SIZE_IN_BITS == 64
instance Cast Word   Word64 where
    cast (W# w) = W64# w
instance Cast Word64 Word where
    cast (W64# w) = W# w

instance Cast Word   Int64 where
    cast (W# w) = I64# (word2Int# w)
instance Cast Int64  Word where
    cast (I64# i) = W# (int2Word# i)

instance Cast Int    Int64 where
    cast (I# i) = I64# i
instance Cast Int64  Int where
    cast (I64# i) = I# i

instance Cast Int    Word64 where
    cast (I# i) = W64# (int2Word# i)
instance Cast Word64 Int where
    cast (W64# w) = I# (word2Int# w)
#else
instance Cast Word   Word32 where
    cast (W# w) = W32# w
instance Cast Word32 Word where
    cast (W32# w) = W# w

instance Cast Word   Int32 where
    cast (W# w) = I32# (word2Int# w)
instance Cast Int32  Word where
    cast (I32# i) = W# (int2Word# i)

instance Cast Int    Int32 where
    cast (I# i) = I32# i
instance Cast Int32  Int where
    cast (I32# i) = I# i

instance Cast Int    Word32 where
    cast (I# i) = W32# (int2Word# i)
instance Cast Word32 Int where
    cast (W32# w) = I# (word2Int# w)
#endif

instance Cast Word64 Double where
    cast = castWord64ToDouble
instance Cast Word32 Float where
    cast = castWord32ToFloat
instance Cast Double Word64 where
    cast = castDoubleToWord64
instance Cast Float Word32 where
    cast = castFloatToWord32

instance Cast Int64 Double where
    cast = castWord64ToDouble . cast
instance Cast Int32 Float where
    cast = castWord32ToFloat . cast
instance Cast Double Int64 where
    cast = cast . castDoubleToWord64
instance Cast Float Int32 where
    cast = cast . castFloatToWord32
