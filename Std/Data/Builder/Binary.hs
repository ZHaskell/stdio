

module Std.Data.Builder.Binary
(
-- * Primitives
  word8
, int8
, bytes

-- * Big-endian primitives
, word16be
, word32be
, word64be
, int16be
, int32be
, int64be
, floatbe
, doublebe

-- * Little-endian primitives
, word16le
, word32le
, word64le
, int16le
, int32le
, int64le
, floatle
, doublele

-- * Host-endian, unaligned writes
, wordhost
, word16host
, word32host
, word64host
, inthost
, int16host
, int32host
, int64host
, floathost
, doublehost

-- * Unicode
, charUtf8
, stringUtf8
) where

import Std.Data.Builder.Base


word8 :: Word8 -> Builder ()
{-# INLINABLE word8 #-}
word8 = encodedPrim

int8 :: Int8 -> Builder ()
{-# INLINABLE word8 #-}
word8 = encodedPrim

--------------------------------------------------------------------------------
-- * Big-endian primitives
word16be
word32be
word64be
int16be
int32be
int64be
floatbe
doublebe

--------------------------------------------------------------------------------
-- * Little-endian primitives
word16le
word32le
word64le
int16le
int32le
int64le
floatle
doublele

--------------------------------------------------------------------------------
-- * Host-endian, unaligned writes
wordhost
word16host
word32host
word64host
inthost
int16host
int32host
int64host
floathost
doublehost

