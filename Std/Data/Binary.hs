{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

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

import           Control.Monad.Primitive (RealWorld)
import           GHC.Word
import           GHC.Int
import           GHC.Prim
import           GHC.Types
import           Data.Primitive.PrimArray
import           Std.Data.Builder
import           Std.Data.Parser
import           Std.Data.Vector

#include "MachDeps.h"

class Encode a where
    encode :: a -> Builder ()

class Decode a where
    decode :: Parser a

-- | Bools are encoded as a byte, 0 for 'False', 1 for 'True'.
instance Encode Bool where
    {-# INLINE encode #-}
    encode False = encodeWord8 0
    encode True  = encodeWord8 1

instance Encode Word8 where
    {-# INLINE encode #-}
    encode = encodeWord8

instance Decode Word8 where
    {-# INLINE decode #-}
    decode = decodeWord8

instance Encode Word16 where
    {-# INLINE encode #-}
    encode = encodeWord16LE

instance Encode Bytes where
    {-# INLINE encode #-}
    encode = undefined -- bytes

--------------------------------------------------------------------------------

encodeWord8 :: Word8 -> Builder ()
{-# INLINE encodeWord8 #-}
encodeWord8 (W8# x#) = encodePrim 1# (\ mba# i# -> writeWord8Array# mba# i# x#)

data Test = Test
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8
  deriving Show

decodeTest :: Parser Test
decodeTest = Test <$> decodeWord8
                  <*> decodeWord8
                  <*> (decodeWord8 `plus` decodeWord8)
--   !w1 <- decodeWord8
--   !w2 <- decodeWord8
--   !w3 <- decodeWord8
--   return (Test w1 w2 w3)

   -- Test <$> decodeWord8
   --               <*> decodeWord8
   --               <*> decodeWord8

decodeWord8 :: Parser Word8
{-# INLINE decodeWord8 #-}
decodeWord8 =
    --decodePrim 1 (\ ba i -> (indexPrimArray ba i))
    decodePrim 1# (\ ba# i# -> W8# (indexWord8Array# ba# i#))

encodeWord16Host :: Word16 -> Builder ()
{-# INLINE encodeWord16Host #-}
encodeWord16Host (W16# x#) = encodePrim 2# (\ mba# i# -> writeWord8ArrayAsWord16# mba# i# x#)

encodeWord16LE :: Word16 -> Builder ()
{-# INLINE encodeWord16LE #-}
encodeWord16LE (W16# x#) =
#ifdef WORDS_BIGENDIAN
    encodePrim 2# (\ mba# i# s0# ->
        let s1# = writeWord8Array# mba# i# x# s0#
        in        writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 8#) s1#)
#else
    encodePrim 2# (\ mba# i# -> writeWord8ArrayAsWord16# mba# i# x#)
#endif

encodeWord16BE :: Word16 -> Builder ()
{-# INLINE encodeWord16BE #-}
encodeWord16BE (W16# x#) =
#ifdef WORDS_BIGENDIAN
    encodePrim 2# (\ mba# i# -> writeWord8ArrayAsWord16# mba# i# x#)
#else
    encodePrim 2# (\ mba# i# s0# ->
        let s1# = writeWord8Array# mba# i# (uncheckedShiftRL# x# 8#) s0#
        in        writeWord8Array# mba# (i# +# 1#) x# s1#)
#endif

encodeWord32Host :: Word32 -> Builder ()
{-# INLINE encodeWord32Host #-}
encodeWord32Host (W32# x#) = encodePrim 4# (\ mba# i# -> writeWord8ArrayAsWord32# mba# i# x#)

encodeWord32LE :: Word32 -> Builder ()
{-# INLINE encodeWord32LE #-}
encodeWord32LE (W32# x#) =
#ifdef WORDS_BIGENDIAN
    encodePrim 4# (\ mba# i# s0# ->
        let s1# = writeWord8Array# mba# i# x# s0#
            s2# = writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 8#) s1#
            s3# = writeWord8Array# mba# (i# +# 2#) (uncheckedShiftRL# x# 16#) s2#
        in        writeWord8Array# mba# (i# +# 3#) (uncheckedShiftRL# x# 24#) s3#)
#else
    encodePrim 4# (\ mba# i# -> writeWord8ArrayAsWord32# mba# i# x#)
#endif

encodeWord32BE :: Word32 -> Builder ()
{-# INLINE encodeWord32BE #-}
encodeWord32BE (W32# x#) =
#ifdef WORDS_BIGENDIAN
    encodePrim 4# (\ mba# i# -> writeWord8ArrayAsWord32# mba# i# x#)
#else
    encodePrim 4# (\ mba# i# s0# ->
        let s1# = writeWord8Array# mba# i# (uncheckedShiftRL# x# 24#) s0#
            s2# = writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 16#) s1#
            s3# = writeWord8Array# mba# (i# +# 2#) (uncheckedShiftRL# x# 8#) s2#
        in        writeWord8Array# mba# (i# +# 3#) x# s3#)
#endif

encodeWord64Host :: Word64 -> Builder ()
{-# INLINE encodeWord64Host #-}
encodeWord64Host (W64# x#) = encodePrim 8# (\ mba# i# -> writeWord8ArrayAsWord64# mba# i# x#)

encodeWord64LE :: Word64 -> Builder ()
{-# INLINE encodeWord64LE #-}
encodeWord64LE (W64# x#) =
#ifdef WORDS_BIGENDIAN
    encodePrim 8# (\ mba# i# s0# ->
        let s1# = writeWord8Array# mba# i# x# s0#
            s2# = writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 8#) s1#
            s3# = writeWord8Array# mba# (i# +# 2#) (uncheckedShiftRL# x# 16#) s2#
            s4# = writeWord8Array# mba# (i# +# 3#) (uncheckedShiftRL# x# 24#) s3#
            s5# = writeWord8Array# mba# (i# +# 4#) (uncheckedShiftRL# x# 32#) s4#
            s6# = writeWord8Array# mba# (i# +# 5#) (uncheckedShiftRL# x# 40#) s5#
            s7# = writeWord8Array# mba# (i# +# 6#) (uncheckedShiftRL# x# 48#) s6#
        in        writeWord8Array# mba# (i# +# 7#) (uncheckedShiftRL# x# 56#) s7#)
#else
    encodePrim 8# (\ mba# i# -> writeWord8ArrayAsWord64# mba# i# x#)
#endif

encodeWord64BE :: Word64 -> Builder ()
{-# INLINE encodeWord64BE #-}
encodeWord64BE (W64# x#) =
#ifdef WORDS_BIGENDIAN
    encodePrim 8# (\ mba# i# -> writeWord8ArrayAsWord64# mba# i# x#)
#else
    encodePrim 8# (\ mba# i# s0# ->
        let s1# = writeWord8Array# mba# i# (uncheckedShiftRL# x# 56#) s0#
            s2# = writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 48#) s1#
            s3# = writeWord8Array# mba# (i# +# 2#) (uncheckedShiftRL# x# 40#) s2#
            s4# = writeWord8Array# mba# (i# +# 3#) (uncheckedShiftRL# x# 32#) s3#
            s5# = writeWord8Array# mba# (i# +# 4#) (uncheckedShiftRL# x# 24#) s4#
            s6# = writeWord8Array# mba# (i# +# 5#) (uncheckedShiftRL# x# 16#) s5#
            s7# = writeWord8Array# mba# (i# +# 6#) (uncheckedShiftRL# x# 8#) s6#
        in        writeWord8Array# mba# (i# +# 7#) x# s7#)
#endif

encodeWordHost :: Word -> Builder ()
{-# INLINE encodeWordHost #-}
encodeWordHost =
#if SIZEOF_HSWORD == 4
    encodeWord32Host . fromIntegral
#else
    encodeWord64Host . fromIntegral
#endif

encodeWordLE :: Word -> Builder ()
{-# INLINE encodeWordLE #-}
encodeWordLE =
#if SIZEOF_HSWORD == 4
    encodeWord32LE . fromIntegral
#else
    encodeWord64LE . fromIntegral
#endif

encodeWordBE :: Word -> Builder ()
{-# INLINE encodeWordBE #-}
encodeWordBE =
#if SIZEOF_HSWORD == 4
    encodeWord32BE . fromIntegral
#else
    encodeWord64BE . fromIntegral
#endif

--------------------------------------------------------------------------------

encodeInt8 :: Int8 -> Builder ()
{-# INLINE encodeInt8 #-}
encodeInt8 (I8# x#) = encodePrim 1# (\ mba# i# -> writeInt8Array# mba# i# x#)

encodeInt16Host :: Int16 -> Builder ()
{-# INLINE encodeInt16Host #-}
encodeInt16Host (I16# x#) = encodePrim 2# (\ mba# i# -> writeWord8ArrayAsInt16# mba# i# x#)

encodeInt16LE :: Int16 -> Builder ()
{-# INLINE encodeInt16LE #-}
encodeInt16LE (I16# x#) =
#ifdef WORDS_BIGENDIAN
    encodePrim 2# (\ mba# i# s0# ->
        let s1# = writeInt8Array# mba# i# x# s0#
        in        writeInt8Array# mba# (i# +# 1#) (uncheckedIShiftRL# x# 8#) s1#)
#else
    encodePrim 2# (\ mba# i# -> writeWord8ArrayAsInt16# mba# i# x#)
#endif

encodeInt16BE :: Int16 -> Builder ()
{-# INLINE encodeInt16BE #-}
encodeInt16BE (I16# x#) =
#ifdef WORDS_BIGENDIAN
    encodePrim 2# (\ mba# i# -> writeWord8ArrayAsInt16# mba# i# x#)
#else
    encodePrim 2# (\ mba# i# s0# ->
        let s1# = writeInt8Array# mba# i# (uncheckedIShiftRL# x# 8#) s0#
        in        writeInt8Array# mba# (i# +# 1#) x# s1#)
#endif

encodeInt32Host :: Int32 -> Builder ()
{-# INLINE encodeInt32Host #-}
encodeInt32Host (I32# x#) = encodePrim 4# (\ mba# i# -> writeWord8ArrayAsInt32# mba# i# x#)

encodeInt32LE :: Int32 -> Builder ()
{-# INLINE encodeInt32LE #-}
encodeInt32LE (I32# x#) =
#ifdef WORDS_BIGENDIAN
    encodePrim 4# (\ mba# i# s0# ->
        let s1# = writeInt8Array# mba# i# x# s0#
            s2# = writeInt8Array# mba# (i# +# 1#) (uncheckedIShiftRL# x# 8#) s1#
            s3# = writeInt8Array# mba# (i# +# 2#) (uncheckedIShiftRL# x# 16#) s2#
        in        writeInt8Array# mba# (i# +# 3#) (uncheckedIShiftRL# x# 24#) s3#)
#else
    encodePrim 4# (\ mba# i# -> writeWord8ArrayAsInt32# mba# i# x#)
#endif

encodeInt32BE :: Int32 -> Builder ()
{-# INLINE encodeInt32BE #-}
encodeInt32BE (I32# x#) =
#ifdef WORDS_BIGENDIAN
    encodePrim 4# (\ mba# i# -> writeWord8ArrayAsInt32# mba# i# x#)
#else
    encodePrim 4# (\ mba# i# s0# ->
        let s1# = writeInt8Array# mba# i# (uncheckedIShiftRL# x# 24#) s0#
            s2# = writeInt8Array# mba# (i# +# 1#) (uncheckedIShiftRL# x# 16#) s1#
            s3# = writeInt8Array# mba# (i# +# 2#) (uncheckedIShiftRL# x# 8#) s2#
        in        writeInt8Array# mba# (i# +# 3#) x# s3#)
#endif

encodeInt64Host :: Int64 -> Builder ()
{-# INLINE encodeInt64Host #-}
encodeInt64Host (I64# x#) = encodePrim 8# (\ mba# i# -> writeWord8ArrayAsInt64# mba# i# x#)

encodeInt64LE :: Int64 -> Builder ()
{-# INLINE encodeInt64LE #-}
encodeInt64LE (I64# x#) =
#ifdef WORDS_BIGENDIAN
    encodePrim 8# (\ mba# i# s0# ->
        let s1# = writeInt8Array# mba# i# x# s0#
            s2# = writeInt8Array# mba# (i# +# 1#) (uncheckedIShiftRL# x# 8#) s1#
            s3# = writeInt8Array# mba# (i# +# 2#) (uncheckedIShiftRL# x# 16#) s2#
            s4# = writeInt8Array# mba# (i# +# 3#) (uncheckedIShiftRL# x# 24#) s3#
            s5# = writeInt8Array# mba# (i# +# 4#) (uncheckedIShiftRL# x# 32#) s4#
            s6# = writeInt8Array# mba# (i# +# 5#) (uncheckedIShiftRL# x# 40#) s5#
            s7# = writeInt8Array# mba# (i# +# 6#) (uncheckedIShiftRL# x# 48#) s6#
        in        writeInt8Array# mba# (i# +# 7#) (uncheckedIShiftRL# x# 56#) s7#)
#else
    encodePrim 8# (\ mba# i# -> writeWord8ArrayAsInt64# mba# i# x#)
#endif

encodeInt64BE :: Int64 -> Builder ()
{-# INLINE encodeInt64BE #-}
encodeInt64BE (I64# x#) =
#ifdef WORDS_BIGENDIAN
    encodePrim 8# (\ mba# i# -> writeWord8ArrayAsInt64# mba# i# x#)
#else
    encodePrim 8# (\ mba# i# s0# ->
        let s1# = writeInt8Array# mba# i# (uncheckedIShiftRL# x# 56#) s0#
            s2# = writeInt8Array# mba# (i# +# 1#) (uncheckedIShiftRL# x# 48#) s1#
            s3# = writeInt8Array# mba# (i# +# 2#) (uncheckedIShiftRL# x# 40#) s2#
            s4# = writeInt8Array# mba# (i# +# 3#) (uncheckedIShiftRL# x# 32#) s3#
            s5# = writeInt8Array# mba# (i# +# 4#) (uncheckedIShiftRL# x# 24#) s4#
            s6# = writeInt8Array# mba# (i# +# 5#) (uncheckedIShiftRL# x# 16#) s5#
            s7# = writeInt8Array# mba# (i# +# 6#) (uncheckedIShiftRL# x# 8#) s6#
        in        writeInt8Array# mba# (i# +# 7#) x# s7#)
#endif

encodeIntHost :: Int -> Builder ()
{-# INLINE encodeIntHost #-}
encodeIntHost =
#if SIZEOF_HSWORD == 4
    encodeInt32Host . fromIntegral
#else
    encodeInt64Host . fromIntegral
#endif

encodeIntLE :: Int -> Builder ()
{-# INLINE encodeIntLE #-}
encodeIntLE =
#if SIZEOF_HSWORD == 4
    encodeInt32LE . fromIntegral
#else
    encodeInt64LE . fromIntegral
#endif

encodeIntBE :: Int -> Builder ()
{-# INLINE encodeIntBE #-}
encodeIntBE =
#if SIZEOF_HSWORD == 4
    encodeInt32BE . fromIntegral
#else
    encodeInt64BE . fromIntegral
#endif

