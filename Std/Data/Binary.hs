{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Std.Data.Binary
Description : Binary serialization/deserialization
Copyright   : (c) Dong Han, 2017
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Efficiently serialization using simple binary encoding. Default instances
use "Intel convention", i.e. little endian encoding, encoded data should be portable
across machine endianness, word size, or compiler version within one major version.
For example, data encoded using the 'Binary' class could be written on any machine,
and read back on any another using @stdio@ packages with the same major version.
-}

module Std.Data.Piston where

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

class ToPiston a where
    encode :: a -> Builder ()

instance ToPiston Bool where
    {-# INLINE encode #-}
    encode False = encodePrim @Word8 0
    encode True  = encodePrim @Word8 1

#define TO_INST(type) \
    instance ToPiston type where { \
        {-# INLINE encode #-}; \
        encode = encodePrimLE; }\

instance ToPiston Word8 where
    {-# INLINE encode #-}
    encode = encodePrim

TO_INST(Word16)
TO_INST(Word32)
TO_INST(Word64)
TO_INST(Word)

--------------------------------------------------------------------------------

class FromPiston a where
    decode :: Parser a

instance FromPiston Word8 where
    {-# INLINE decode #-}
    decode = decodePrim

#define FROM_INST(type) \
    instance FromPiston type where { \
        {-# INLINE decode #-}; \
        decode = decodePrimLE; }\

FROM_INST(Word16)
FROM_INST(Word32)
FROM_INST(Word64)
FROM_INST(Word)
