{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Std.Data.Binary
Description : Binary serialization/deserialization
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable
Efficiently serialization using simple binary encoding. Default instances
use "Intel convention", i.e. little endian encoding, encoded data should be portable
across machine endianness, word size, or compiler version within one major version.
For example, data encoded using the 'Binary' class could be written on any machine,
and read back on any another using @stdio@ packages with the same major version.
-}

module Std.Data.Binary where

import           GHC.Word
import           GHC.Int
import           GHC.Prim
import           GHC.Types
import           Data.Primitive.PrimArray
import           Std.Data.Builder
import           Std.Data.Parser
import           Std.Data.Vector
import           Std.Data.PrimArray.UnalignedAccess

#include "MachDeps.h"

class Encode a where
    encode :: a -> Builder ()

instance Encode Bool where
    {-# INLINE encode #-}
    encode False = encodePrim @Word8 0
    encode True  = encodePrim @Word8 1

#define ENCODE_INST(type) \
    instance Encode type where { \
        {-# INLINE encode #-}; \
        encode = encodePrimBE; }\

instance Encode Word8 where
    {-# INLINE encode #-}
    encode = encodePrim

ENCODE_INST(Word16)
ENCODE_INST(Word32)
ENCODE_INST(Word64)
ENCODE_INST(Word)

--------------------------------------------------------------------------------

class Decode a where
    decode :: Parser a

instance Decode Word8 where
    {-# INLINE decode #-}
    decode = decodePrim

#define DECODE_INST(type) \
    instance Decode type where { \
        {-# INLINE decode #-}; \
        decode = decodePrimBE; }\

DECODE_INST(Word16)
DECODE_INST(Word32)
DECODE_INST(Word64)
DECODE_INST(Word)
