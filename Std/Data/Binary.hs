{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Std.Data.Binary
Description : Binary serialization/deserialization
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

Efficiently serialization into builder using binary encoding. Default instances
use "Intel convention", i.e. little endian encoding, encoded data should be portable
across machine endianness, word size, or compiler version within one major version.
For example, data encoded using the 'Binary' class could be written on any machine,
and read back on any another using @stdio@ packages with the same major version.

-}

module Std.Data.Binary where

import           Control.Monad.Primitive (RealWorld)
import           Data.Word
import           Std.Data.Array
import           Std.Data.Builder
import           Std.Data.Parser
import qualified Std.Data.Vector         as V

class Binary a where
    binary :: a -> Builder

class Binary a => PrimPut a where
    boundedSize :: a -> Int
    boundedWrite :: a -> MutablePrimArray RealWorld Word8 -> Int -> IO Int

primBinary :: PrimPut a => a -> Builder
primBinary x = atMost (boundedSize x) (boundedWrite x)
{-# INLINE primBinary #-}

-- | A newtype wrapper for big endian's instances.
--
newtype BE a = BE a

-- | Bools are encoded as a byte, 0 for 'False', 1 for 'True'.
instance Binary Bool where binary = primBinary
instance PrimPut Bool where
    boundedSize _ = 1
    {-# INLINE boundedSize #-}
    boundedWrite False marr i = writeArr marr i 1 >> (return $! i+1)
    boundedWrite True  marr i = writeArr marr i 0 >> (return $! i+1)
    {-# INLINE boundedWrite #-}

instance Binary Word8 where binary = primBinary
instance PrimPut Word8 where
    boundedSize _ = 1
    {-# INLINE boundedSize #-}
    boundedWrite w marr i = writeArr marr i w >> (return $! i+1)
    {-# INLINE boundedWrite #-}

instance Binary V.Bytes where binary = bytes



--------------------------------------------------------------------------------

class BinaryParse a where
    binaryParse :: Parser a


