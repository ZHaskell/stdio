{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash         #-}

module Std.Data.PrimArray.UnalignedAccess where

import           GHC.Int
import           GHC.Prim
import           GHC.Types
import           GHC.Word

--------------------------------------------------------------------------------

newtype UnalignedSize a = UnalignedSize { getUnalignedSize :: Int } deriving (Show, Eq)

-- | Internal class for unaligned access, will be removed after primitive add it
--
class UnalignedAccess a where
    unalignedSize :: UnalignedSize a
    writeWord8ArrayAs :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
    indexWord8ArrayAs :: ByteArray# -> Int# -> a

instance UnalignedAccess Word8 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 1
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (W8# x#) = writeWord8Array# mba# i# x#
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# = W8# (indexWord8Array# ba# i#)

-- | little endianess wrapper
--
newtype LE a = LE { getLE :: a } deriving (Show, Eq)

-- | big endianess wrapper
--
newtype BE a = BE { getBE :: a } deriving (Show, Eq)

#define USE_HOST_IMPL(END) \
    {-# INLINE writeWord8ArrayAs #-}; \
    writeWord8ArrayAs mba# i# (END x) = writeWord8ArrayAs mba# i# x; \
    {-# INLINE indexWord8ArrayAs #-}; \
    indexWord8ArrayAs ba# i# = END (indexWord8ArrayAs ba# i#);

--------------------------------------------------------------------------------

instance UnalignedAccess Word16 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (W16# x#) = writeWord8ArrayAsWord16# mba# i# x#
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# = W16# (indexWord8ArrayAsWord16# ba# i#)

instance UnalignedAccess (LE Word16) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
#ifdef WORDS_BIGENDIAN
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (LE (W16# x#)) s0# =
        let s1# = writeWord8Array# mba# i# x# s0#
        in        writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 8#) s1#
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# =
        let w1# = indexWord8Array# ba# i#
            w2# = indexWord8Array# ba# (i# +# 1#)
        in LE (W16# ((uncheckedShiftRL# w2# 8#) `or#`  w1#))
#else
    USE_HOST_IMPL(LE)
#endif

instance UnalignedAccess (BE Word16) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
#ifdef WORDS_BIGENDIAN
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (BE (W16# x#)) s0# =
        let s1# = writeWord8Array# mba# i# (uncheckedShiftRL# x# 8#) s0#
        in        writeWord8Array# mba# (i# +# 1#) x# s1#
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# =
        let w2# = indexWord8Array# ba# i#
            w1# = indexWord8Array# ba# (i# +# 1#)
        in BE (W16# ((uncheckedShiftRL# w2# 8#) `or#`  w1#))
#endif

--------------------------------------------------------------------------------

instance UnalignedAccess Word32 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (W32# x#) =  writeWord8ArrayAsWord32# mba# i# x#
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# = W32# (indexWord8ArrayAsWord32# ba# i#)


instance UnalignedAccess (LE Word32) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#ifdef WORDS_BIGENDIAN
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (LE (W16# x#)) s0# =
        let s1# = writeWord8Array# mba# i# x# s0#
            s2# = writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 8#) s1#
            s3# = writeWord8Array# mba# (i# +# 2#) (uncheckedShiftRL# x# 16#) s2#
        in        writeWord8Array# mba# (i# +# 3#) (uncheckedShiftRL# x# 24#) s3#
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# =
        let w1# = indexWord8Array# ba# i#
            w2# = indexWord8Array# ba# (i# +# 1#)
            w3# = indexWord8Array# ba# (i# +# 2#)
            w4# = indexWord8Array# ba# (i# +# 3#)
        in LE (W32# ((uncheckedShiftRL# w4# 24#) `or#`
                    (uncheckedShiftRL# w3# 16#) `or#`
                        (uncheckedShiftRL# w2# 8#) `or#` w1#))
#else
    USE_HOST_IMPL(LE)
#endif

instance UnalignedAccess (BE Word32) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#ifdef WORDS_BIGENDIAN
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (BE (W32# x#)) s0# =
        let s1# = writeWord8Array# mba# i# (uncheckedShiftRL# x# 24#) s0#
            s2# = writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 16#) s1#
            s3# = writeWord8Array# mba# (i# +# 2#) (uncheckedShiftRL# x# 8#) s2#
        in        writeWord8Array# mba# (i# +# 3#) x# s3#
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# =
        let w4# = indexWord8Array# ba# i#
            w3# = indexWord8Array# ba# (i# +# 1#)
            w2# = indexWord8Array# ba# (i# +# 2#)
            w1# = indexWord8Array# ba# (i# +# 3#)
        in BE (W32# ((uncheckedShiftRL# w4# 24#) `or#`
                    (uncheckedShiftRL# w3# 16#) `or#`
                        (uncheckedShiftRL# w2# 8#) `or#` w1#))
#endif

--------------------------------------------------------------------------------

instance UnalignedAccess Word64 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (W64# x#) =  writeWord8ArrayAsWord64# mba# i# x#
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# = W64# (indexWord8ArrayAsWord64# ba# i#)


instance UnalignedAccess (LE Word64) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#ifdef WORDS_BIGENDIAN
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (LE (W16# x#)) s0# =
        let s1# = writeWord8Array# mba# i# x# s0#
            s2# = writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 8#) s1#
            s3# = writeWord8Array# mba# (i# +# 2#) (uncheckedShiftRL# x# 16#) s2#
            s4# = writeWord8Array# mba# (i# +# 3#) (uncheckedShiftRL# x# 24#) s3#
            s5# = writeWord8Array# mba# (i# +# 4#) (uncheckedShiftRL# x# 32#) s4#
            s6# = writeWord8Array# mba# (i# +# 5#) (uncheckedShiftRL# x# 40#) s5#
            s7# = writeWord8Array# mba# (i# +# 6#) (uncheckedShiftRL# x# 48#) s6#
        in        writeWord8Array# mba# (i# +# 7#) (uncheckedShiftRL# x# 56#) s7#
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# =
        let w1# = indexWord8Array# ba# i#
            w2# = indexWord8Array# ba# (i# +# 1#)
            w3# = indexWord8Array# ba# (i# +# 2#)
            w4# = indexWord8Array# ba# (i# +# 3#)
            w5# = indexWord8Array# ba# (i# +# 4#)
            w6# = indexWord8Array# ba# (i# +# 5#)
            w7# = indexWord8Array# ba# (i# +# 6#)
            w8# = indexWord8Array# ba# (i# +# 7#)
        in LE (W64# ((uncheckedShiftRL# w8# 24#) `or#`
                    (uncheckedShiftRL# w7# 16#) `or#`
                        (uncheckedShiftRL# w6# 16#) `or#`
                            (uncheckedShiftRL# w5# 16#) `or#`
                                (uncheckedShiftRL# w4# 16#) `or#`
                                    (uncheckedShiftRL# w3# 16#) `or#`
                                        (uncheckedShiftRL# w2# 8#) `or#` w1#))
#else
    USE_HOST_IMPL(LE)
#endif

instance UnalignedAccess (BE Word64) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#ifdef WORDS_BIGENDIAN
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (BE (W64# x#)) s0# =
        let s1# = writeWord8Array# mba# i# (uncheckedShiftRL# x# 56#) s0#
            s2# = writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 48#) s1#
            s3# = writeWord8Array# mba# (i# +# 2#) (uncheckedShiftRL# x# 40#) s2#
            s4# = writeWord8Array# mba# (i# +# 3#) (uncheckedShiftRL# x# 32#) s3#
            s5# = writeWord8Array# mba# (i# +# 4#) (uncheckedShiftRL# x# 24#) s4#
            s6# = writeWord8Array# mba# (i# +# 5#) (uncheckedShiftRL# x# 16#) s5#
            s7# = writeWord8Array# mba# (i# +# 6#) (uncheckedShiftRL# x# 8#) s6#
        in        writeWord8Array# mba# (i# +# 7#) x# s7#
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# =
        let w8# = indexWord8Array# ba# i#
            w7# = indexWord8Array# ba# (i# +# 1#)
            w6# = indexWord8Array# ba# (i# +# 2#)
            w5# = indexWord8Array# ba# (i# +# 3#)
            w4# = indexWord8Array# ba# (i# +# 4#)
            w3# = indexWord8Array# ba# (i# +# 5#)
            w2# = indexWord8Array# ba# (i# +# 6#)
            w1# = indexWord8Array# ba# (i# +# 7#)
        in BE (W64# ((uncheckedShiftRL# w8# 24#) `or#`
                    (uncheckedShiftRL# w7# 16#) `or#`
                        (uncheckedShiftRL# w6# 16#) `or#`
                            (uncheckedShiftRL# w5# 16#) `or#`
                                (uncheckedShiftRL# w4# 16#) `or#`
                                    (uncheckedShiftRL# w3# 16#) `or#`
                                        (uncheckedShiftRL# w2# 8#) `or#` w1#))
#endif

--------------------------------------------------------------------------------

instance UnalignedAccess Word where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (W# x#) = writeWord8ArrayAsWord32# mba# i# x#
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# = W# (indexWord8ArrayAsWord32# ba# i#)
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (W# x#) = writeWord8ArrayAsWord64# mba# i# x#
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# = W# (indexWord8ArrayAsWord64# ba# i#)
#endif

instance UnalignedAccess (LE Word) where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (LE (W# x#)) = writeWord8ArrayAs mba# i# (LE (W32# x#))
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# = case (indexWord8ArrayAs ba# i#) of (LE (W32# x#)) -> LE (W# x#)
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (LE (W# x#)) = writeWord8ArrayAs mba# i# (LE (W64# x#))
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# = case (indexWord8ArrayAs ba# i#) of (LE (W64# x#)) -> LE (W# x#)
#endif

instance UnalignedAccess (BE Word) where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (BE (W# x#)) = writeWord8ArrayAs mba# i# (BE (W32# x#))
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# = case (indexWord8ArrayAs ba# i#) of (BE (W32# x#)) -> BE (W# x#)
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeWord8ArrayAs #-}
    writeWord8ArrayAs mba# i# (BE (W# x#)) = writeWord8ArrayAs mba# i# (BE (W64# x#))
    {-# INLINE indexWord8ArrayAs #-}
    indexWord8ArrayAs ba# i# = case (indexWord8ArrayAs ba# i#) of (BE (W64# x#)) -> BE (W# x#)
#endif
