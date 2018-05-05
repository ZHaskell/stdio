{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-- | This module implement some bit twiddling with ghc primitives.
--
-- We currently didn't use all functions from this module though: the performance is not
-- catching up c version yet. But this module and relevant benchmarks are kept in hope
-- that once we have fully SIMD support in GHC, we might optimize these functions further
-- to compete with c.
--
-- Reference:
--
-- * https://graphics.stanford.edu/~seander/bithacks.html
-- * https://jameshfisher.github.io/2017/01/24/bitwise-check-for-zero-byte.html
--
--
module Data.Primitive.BitTwiddle where

import GHC.Prim
import GHC.Types
import GHC.Word
import Data.Primitive.PrimArray
import Data.Primitive.ByteArray

-- we need to know word size
#include "MachDeps.h"

#if SIZEOF_HSWORD == 4
# define CAST_OFFSET_WORD_TO_BYTE(x) (x `uncheckedIShiftL#` 2#)
# define CAST_OFFSET_BYTE_TO_WORD(x) (x `uncheckedIShiftRA#` 2#)
#else
# define CAST_OFFSET_WORD_TO_BYTE(x) (x `uncheckedIShiftL#` 3#)
# define CAST_OFFSET_BYTE_TO_WORD(x) (x `uncheckedIShiftRA#` 3#)
#endif

isOffsetAligned# :: Int# -> Bool
{-# INLINE isOffsetAligned# #-}
isOffsetAligned# s# = isTrue# ((SIZEOF_HSWORD# -# 1#) `andI#` s# ==# 0#)

mkMask# :: Word# -> Word#
{-# INLINE mkMask# #-}
mkMask# w8# =
#if SIZEOF_HSWORD == 4
    let w16# = w8# `or#` (w8# `uncheckedShiftL#` 8#)
    in w16# `or#` (w16# `uncheckedShiftL#` 16#)
#else
    let w16# = w8# `or#` (w8# `uncheckedShiftL#` 8#)
        w32# = w16# `or#` (w16# `uncheckedShiftL#` 16#)
    in w32# `or#` (w32# `uncheckedShiftL#` 32#)
#endif

-- https://jameshfisher.github.io/2017/01/24/bitwise-check-for-zero-byte.html
--
nullByteMagic# :: Word# -> Word#
{-# INLINE nullByteMagic# #-}
nullByteMagic# w# =
#if SIZEOF_HSWORD == 4
    (w# `minusWord#` 0x01010101##) `and#` (not# w#) `and#` 0x80808080##
#else
    (w# `minusWord#` 0x0101010101010101##) `and#` (not# w#) `and#` 0x8080808080808080##
#endif

-- | Search a word8 in array.
--
-- Currently this function is ~4 times slow than c version, so we didn't use it.
--
memchr :: PrimArray Word8 -- array
       -> Word8           -- target
       -> Int             -- start offset
       -> Int             -- search length
       -> Int
{-# INLINE memchr #-}
memchr (PrimArray (ByteArray ba#)) (W8# c#) (I# s#) (I# siz#) =
    I# (memchr# ba# c# s# siz#)

-- | The unboxed version of 'memchr'
--
memchr# :: ByteArray# -> Word# -> Int# -> Int# -> Int#
{-# NOINLINE memchr# #-}
memchr# ba# c# s# siz# = beforeAlignedLoop# ba# c# s# (s# +# siz#)
  where
    beforeAlignedLoop# :: ByteArray# -> Word# -> Int# -> Int# -> Int#
    beforeAlignedLoop# ba# c# s# end#
        | isTrue# (s# >=# end#) = -1#
        | isTrue# (c# `eqWord#` indexWord8Array# ba# s#) = s#
        | isOffsetAligned# s# = alignedLoop# ba# (mkMask# c#)
                                           CAST_OFFSET_BYTE_TO_WORD(s#)
                                           CAST_OFFSET_BYTE_TO_WORD(end#)
                                           end#
        | otherwise = beforeAlignedLoop# ba# c# (s# +# 1#) end#

    alignedLoop# :: ByteArray# -> Word# -> Int# -> Int# -> Int# -> Int#
    alignedLoop# ba# mask# s# end# end_#
        | isTrue# (s# >=# end#) = afterAlignedLoop# ba# (mask# `and#` 0xFF##)
                                                    CAST_OFFSET_WORD_TO_BYTE(s#)
                                                    end_#
        | otherwise = case indexWordArray# ba# s# of
            w# ->
                case nullByteMagic# (mask# `xor#` w#) of
                    0## -> alignedLoop# ba# mask# (s# +# 1#) end# end_#
                    _   -> afterAlignedLoop# ba# (mask# `and#` 0xFF##)
                                             CAST_OFFSET_WORD_TO_BYTE(s#)
                                             end_#

    afterAlignedLoop# :: ByteArray# -> Word# -> Int# -> Int# -> Int#
    afterAlignedLoop# ba# c# s# end#
        | isTrue# (s# >=# end#) = -1#
        | isTrue# (c# `eqWord#` indexWord8Array# ba# s#) = s#
        | otherwise = afterAlignedLoop# ba# c# (s# +# 1#) end#

-- | Search a word8 array in reverse order.
--
-- This function is used in @elemIndexEnd@, since there's no c equivalent.
--
memchrReverse :: PrimArray Word8  -- array
              -> Word8            -- target
              -> Int              -- start offset
              -> Int              -- search length
              -> Int
{-# INLINE memchrReverse #-}
memchrReverse (PrimArray (ByteArray ba#)) (W8# c#) (I# s#) (I# siz#) =
    I# (memchr# ba# c# s# siz#)

-- | The unboxed version of 'memchrReverse'
--
memchrReverse# :: ByteArray# -> Word# -> Int# -> Int# -> Int#
{-# NOINLINE memchrReverse# #-}
memchrReverse# ba# c# s# siz# = beforeAlignedLoop# ba# c# s# (s# -# siz#)
  where
    beforeAlignedLoop# :: ByteArray# -> Word# -> Int# -> Int# -> Int#
    beforeAlignedLoop# ba# c# s# end#
        | isTrue# (s# <# end#) = -1#
        | isTrue# (c# `eqWord#` indexWord8Array# ba# s#) = s#
        | isOffsetAligned# s# = alignedLoop# ba# (mkMask# c#)
                                           CAST_OFFSET_BYTE_TO_WORD(s#)
                                           CAST_OFFSET_BYTE_TO_WORD(end#)
                                           end#
        | otherwise = beforeAlignedLoop# ba# c# (s# -# 1#) end#

    alignedLoop# :: ByteArray# -> Word# -> Int# -> Int# -> Int# -> Int#
    alignedLoop# ba# mask# s# end# end_#
        | isTrue# (s# <# end#) = afterAlignedLoop# ba# (mask# `and#` 0xFF##)
                                                   CAST_OFFSET_WORD_TO_BYTE(s#)
                                                   end_#
        | otherwise = case indexWordArray# ba# s# of
            w# ->
                case nullByteMagic# (mask# `xor#` w#) of
                    0## -> alignedLoop# ba# mask# (s# -# 1#) end# end_#
                    _   -> afterAlignedLoop# ba# (mask# `and#` 0xFF##)
                                             CAST_OFFSET_WORD_TO_BYTE(s#)
                                             end_#

    afterAlignedLoop# :: ByteArray# -> Word# -> Int# -> Int# -> Int#
    afterAlignedLoop# ba# c# s# end#
        | isTrue# (s# <# end#) = -1#
        | isTrue# (c# `eqWord#` indexWord8Array# ba# s#) = s#
        | otherwise = afterAlignedLoop# ba# c# (s# -# 1#) end#
