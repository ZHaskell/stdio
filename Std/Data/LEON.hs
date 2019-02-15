{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

#if __GLASGOW_HASKELL__ >= 800
#define HAS_DATA_KIND
#endif

{-|
Module      : Std.Data.LEON
Description : Simple binary serialization/deserialization
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

LEON (little endian object notation) is an efficiently serialization using simple binary encoding. As suggested by its name, default instances use little endian encoding, i.e. "Intel convention", encoded data should be portable
across machine endianness, word size, or compiler version within one major version. For example, data encoded using the 'LEON' class could be written on any machine, and read back on any another using @stdio@ packages with the same major version.

This module is still W.I.P, there're lots of instances to be added.

-}

module Std.Data.LEON where

import           Data.Bits
import           Data.Primitive.PrimArray
import           GHC.Generics
import           GHC.Int
import           GHC.Prim
import           GHC.Types
import           GHC.Word
import           Std.Data.Builder
import           Std.Data.Parser
import           Std.Data.PrimArray.UnalignedAccess
import           Std.Data.Vector

#include "MachDeps.h"

-- Factored into two classes because this makes GHC optimize the
-- instances faster.  This doesn't matter for builds of stdio,
-- but it matters a lot for end-users who write 'instance LEON T'.
-- See also: https://ghc.haskell.org/trac/ghc/ticket/9630
class GLEONEncode f where
    gencode :: f t -> Builder ()

class GLEONDecode f where
    gdecode :: Parser (f t)

-- | LEON, \L\ittle Endian Object Notation.
class LEON a where
    encode :: a -> Builder ()
    decode :: Parser a

    default encode :: (Generic a, GLEONEncode (Rep a)) => a -> Builder ()
    encode = gencode . from

    default decode :: (Generic a, GLEONDecode (Rep a)) => Parser a
    decode = to `fmap` gdecode

instance LEON Word8 where
    {-# INLINE encode #-}
    encode = encodePrim
    {-# INLINE decode #-}
    decode = decodePrim

instance LEON Int8 where
    {-# INLINE encode #-}
    encode = encodePrim
    {-# INLINE decode #-}
    decode = decodePrim

instance LEON Bool where
    {-# INLINE encode #-}
    encode False = encodePrim @Word8 0
    encode True  = encodePrim @Word8 1
    {-# INLINE decode #-}
    decode = decodePrim @Word8 >>= \ case 0 -> return False
                                          _ -> return True

#define TO_INST(type) instance LEON type where \
    {-# INLINE encode #-}; \
    encode = encodePrimLE; \
    {-# INLINE decode #-}; \
    decode = decodePrimLE; \

TO_INST(Word16)
TO_INST(Word32)
TO_INST(Word64)
TO_INST(Word)
TO_INST(Int16)
TO_INST(Int32)
TO_INST(Int64)
TO_INST(Int)
TO_INST(Float)
TO_INST(Double)
TO_INST(Char)

--------------------------------------------------------------------------------


-- Type without constructors
instance GLEONEncode V1 where
    gencode _ = pure ()

instance GLEONDecode V1 where
    gdecode   = return undefined

-- Constructor without arguments
instance GLEONEncode U1 where
    gencode U1 = pure ()

instance GLEONDecode U1 where
    gdecode    = return U1

-- Product: constructor with parameters
instance (GLEONEncode a, GLEONEncode b) => GLEONEncode (a :*: b) where
    gencode (x :*: y) = gencode x <> gencode y

instance (GLEONDecode a, GLEONDecode b) => GLEONDecode (a :*: b) where
    gdecode = (:*:) <$> gdecode <*> gdecode

-- Metadata (constructor name, etc)
instance GLEONEncode a => GLEONEncode (M1 i c a) where
    gencode = gencode . unM1

instance GLEONDecode a => GLEONDecode (M1 i c a) where
    gdecode = M1 <$> gdecode

-- Constants, additional parameters, and rank-1 recursion
instance LEON a => GLEONEncode (K1 i a) where
    gencode = encode . unK1

instance LEON a => GLEONDecode (K1 i a) where
    gdecode = K1 <$> decode

-- Borrowed from the cereal package.

-- The following GLEON instance for sums has support for serializing
-- types with up to 2^64-1 constructors. It will use the minimal
-- number of bytes needed to encode the constructor. For example when
-- a type has 2^8 constructors or less it will use a single byte to
-- encode the constructor. If it has 2^16 constructors or less it will
-- use two bytes, and so on till 2^64-1.

#define GUARD(WORD) (size - 1) <= fromIntegral (maxBound :: WORD)
#define PUTSUM(WORD) GUARD(WORD) = encodeSum (0 :: WORD) (fromIntegral size)
#define GETSUM(WORD) GUARD(WORD) = (decode :: Parser WORD) >>= checkGetSum (fromIntegral size)

instance ( GSumEncode  a, GSumEncode  b
         , SumSize    a, SumSize    b) => GLEONEncode (a :+: b) where
    gencode | PUTSUM(Word8) | PUTSUM(Word16) | PUTSUM(Word32) | PUTSUM(Word64)
         | otherwise = sizeError "encode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)

instance ( GSumDecode  a, GSumDecode  b
         , SumSize    a, SumSize    b) => GLEONDecode (a :+: b) where
    gdecode | GETSUM(Word8) | GETSUM(Word16) | GETSUM(Word32) | GETSUM(Word64)
         | otherwise = sizeError "decode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)

sizeError :: Show size => String -> size -> error
sizeError s size =
    error $ "Can't " ++ s ++ " a type with " ++ show size ++ " constructors"

------------------------------------------------------------------------

checkGetSum :: (Ord word, Num word, Bits word, GSumDecode f)
            => word -> word -> Parser (f a)
checkGetSum size code | code < size = decodeSum code size
                      | otherwise   = fail "Unknown encoding for constructor"
{-# INLINE checkGetSum #-}

class GSumDecode f where
    decodeSum :: (Ord word, Num word, Bits word) => word -> word -> Parser (f a)

class GSumEncode f where
    encodeSum :: (Num w, Bits w, LEON w) => w -> w -> f a -> Builder ()

instance (GSumDecode a, GSumDecode b) => GSumDecode (a :+: b) where
    decodeSum !code !size | code < sizeL = L1 <$> decodeSum code           sizeL
                          | otherwise    = R1 <$> decodeSum (code - sizeL) sizeR
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL

instance (GSumEncode a, GSumEncode b) => GSumEncode (a :+: b) where
    encodeSum !code !size s = case s of
                             L1 x -> encodeSum code           sizeL x
                             R1 x -> encodeSum (code + sizeL) sizeR x
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL

instance GLEONDecode a => GSumDecode (C1 c a) where
    decodeSum _ _ = gdecode

instance GLEONEncode a => GSumEncode (C1 c a) where
    encodeSum !code _ x = encode code <> gencode x

------------------------------------------------------------------------

class SumSize f where
    sumSize :: Tagged f Word64

#ifdef HAS_DATA_KIND
newtype Tagged (s :: Type -> Type) b = Tagged {unTagged :: b}
#else
newtype Tagged (s :: * -> *)       b = Tagged {unTagged :: b}
#endif

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a Word64) +
                       unTagged (sumSize :: Tagged b Word64)

instance SumSize (C1 c a) where
    sumSize = Tagged 1
