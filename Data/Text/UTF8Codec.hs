{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module Data.Text.UTF8Codec where

import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import GHC.Prim
import Control.Monad.Primitive
import GHC.Types
import GHC.ST
import GHC.Word


-- | Return a codepoint's encoded length in bytes
--
-- If the codepoint is invalid, we return 3(encoded bytes length of replacement char @\U+FFFD@).
--
encodeCharLength :: Char -> Int
{-# INLINE encodeCharLength #-}
encodeCharLength n
    | n <= '\x00007F' = 1
    | n <= '\x0007FF' = 2
    | n <= '\x00FFFF' = 3
    | n <= '\x10FFFF' = 4
    | otherwise = 3

-- | Encode a 'Char' into bytes
--
-- Write @\U+FFFD@ (encoded as @EF BF BD@ 3 bytes) for invalid unicode codepoint.
-- This function assumed there're enough space for encoded bytes, and return the advanced index.
--
encodeChar :: MutablePrimArray s Word8 -> Int -> Char -> ST s Int
{-# INLINE encodeChar #-}
encodeChar (MutablePrimArray (MutableByteArray mba#)) (I# i#) (C# c#) = ST (\ s# ->
    let (# s1#, j# #) = encodeChar# mba# i# c# s# in (# s1#, (I# j#) #))

-- | The unboxed version of 'encodeChar'
--
-- This function is marked as @NOINLINE@ to reduce code size.
--
encodeChar# :: MutableByteArray# s -> Int# -> Char# -> State# s -> (# State# s, Int# #)
{-# NOINLINE encodeChar# #-} -- codesize vs speed choice here
encodeChar# mba# i# c# = case (int2Word# (ord# c#)) of
    n#
        | isTrue# (n# `leWord#` 0x0000007F##) -> \ s# ->
            let s1# = writeWord8Array# mba# i# n# s#
            in (# s1#, i# +# 1# #)
        | isTrue# (n# `leWord#` 0x000007FF##) -> \ s# ->
            let s1# = writeWord8Array# mba# i# (0xC0## `or#` (n# `uncheckedShiftRL#` 6#)) s#
                s2# = writeWord8Array# mba# (i# +# 1#) (0x80## `or#` (n# `and#` 0x3F##)) s1#
            in (# s2#, i# +# 2# #)
        | isTrue# (n# `leWord#` 0x0000D7FF##) -> \ s# ->
            let s1# = writeWord8Array# mba# i# (0xE0## `or#` (n# `uncheckedShiftRL#` 12#)) s#
                s2# = writeWord8Array# mba# (i# +# 1#) (0x80## `or#` ((n# `uncheckedShiftRL#` 6#) `and#` 0x3F##)) s1#
                s3# = writeWord8Array# mba# (i# +# 2#) (0x80## `or#` (n# `and#` 0x3F##)) s2#
            in (# s3#, i# +# 3# #)
        | isTrue# (n# `leWord#` 0x0000DFFF##) -> \ s# -> -- write replacement char \U+FFFD
            let s1# = writeWord8Array# mba# i# 0xEF## s#
                s2# = writeWord8Array# mba# (i# +# 1#) 0xBF## s1#
                s3# = writeWord8Array# mba# (i# +# 2#) 0xBD## s2#
            in (# s3#, i# +# 3# #)
        | isTrue# (n# `leWord#` 0x0000FFFF##) -> \ s# ->
            let s1# = writeWord8Array# mba# i# (0xE0## `or#` (n# `uncheckedShiftRL#` 12#)) s#
                s2# = writeWord8Array# mba# (i# +# 1#) (0x80## `or#` ((n# `uncheckedShiftRL#` 6#) `and#` 0x3F##)) s1#
                s3# = writeWord8Array# mba# (i# +# 2#) (0x80## `or#` (n# `and#` 0x3F##)) s2#
            in (# s3#, i# +# 3# #)
        | isTrue# (n# `leWord#` 0x0010FFFF##) -> \ s# ->
            let s1# = writeWord8Array# mba# i# (0xF0## `or#` (n# `uncheckedShiftRL#` 18#)) s#
                s2# = writeWord8Array# mba# (i# +# 1#) (0x80## `or#` ((n# `uncheckedShiftRL#` 12#) `and#` 0x3F##)) s1#
                s3# = writeWord8Array# mba# (i# +# 2#) (0x80## `or#` ((n# `uncheckedShiftRL#` 6#) `and#` 0x3F##)) s2#
                s4# = writeWord8Array# mba# (i# +# 3#) (0x80## `or#` (n# `and#` 0x3F##)) s3#
            in (# s4#, i# +# 4# #)
        | otherwise -> \ s# -> -- write replacement char \U+FFFD
            let s1# = writeWord8Array# mba# i#  0xEF## s#
                s2# = writeWord8Array# mba# (i# +# 1#) 0xBF## s1#
                s3# = writeWord8Array# mba# (i# +# 2#) 0xBD## s2#
            in (# s3#, i# +# 3# #)


-- | Encode a 'Char' into bytes with non-standard UTF-8 encoding(Used in "Data.CBytes").
--
-- '\NUL' is encoded as two bytes @C0 80@ , '\xD800' ~ '\xDFFF' is encoded as a three bytes normal utf-8 codepoint.
-- This function assumed there're enough space for encoded bytes, and return the advanced index.
--
encodeCBytesChar :: MutablePrimArray RealWorld Word8 -> Int -> Char -> IO Int
{-# INLINE encodeCBytesChar #-}
encodeCBytesChar (MutablePrimArray (MutableByteArray mba#)) (I# i#) (C# c#) = primitive (\ s# ->
    let (# s1#, j# #) = encodeCBytesChar# mba# i# c# s# in (# s1#, (I# j#) #))

encodeCBytesChar# :: MutableByteArray# s -> Int# -> Char# -> State# s -> (# State# s, Int# #)
{-# NOINLINE encodeCBytesChar# #-} -- codesize vs speed choice here
encodeCBytesChar# mba# i# c# = case (int2Word# (ord# c#)) of
    n#
        | isTrue# (n# `leWord#` 0x0000007F##) -> \ s# ->
            let s1# = writeWord8Array# mba# i# n# s#
            in (# s1#, i# +# 1# #)
        | isTrue# (n# `leWord#` 0x000007FF##) -> \ s# ->
            let s1# = writeWord8Array# mba# i# (0xC0## `or#` (n# `uncheckedShiftRL#` 6#)) s#
                s2# = writeWord8Array# mba# (i# +# 1#) (0x80## `or#` (n# `and#` 0x3F##)) s1#
            in (# s2#, i# +# 2# #)
        | isTrue# (n# `leWord#` 0x0000FFFF##) -> \ s# ->
            let s1# = writeWord8Array# mba# i# (0xE0## `or#` (n# `uncheckedShiftRL#` 12#)) s#
                s2# = writeWord8Array# mba# (i# +# 1#) (0x80## `or#` ((n# `uncheckedShiftRL#` 6#) `and#` 0x3F##)) s1#
                s3# = writeWord8Array# mba# (i# +# 2#) (0x80## `or#` (n# `and#` 0x3F##)) s2#
            in (# s3#, i# +# 3# #)
        | otherwise -> \ s# ->
            let s1# = writeWord8Array# mba# i# (0xF0## `or#` (n# `uncheckedShiftRL#` 18#)) s#
                s2# = writeWord8Array# mba# (i# +# 1#) (0x80## `or#` ((n# `uncheckedShiftRL#` 12#) `and#` 0x3F##)) s1#
                s3# = writeWord8Array# mba# (i# +# 2#) (0x80## `or#` ((n# `uncheckedShiftRL#` 6#) `and#` 0x3F##)) s2#
                s4# = writeWord8Array# mba# (i# +# 3#) (0x80## `or#` (n# `and#` 0x3F##)) s3#
            in (# s4#, i# +# 4# #)

-- | Decode a 'Char' from bytes
--
-- This function assumed all bytes are UTF-8 encoded, and the index param point to the
-- beginning of a codepoint, the decoded character and the advancing offset are returned.
--
-- It's annoying to use unboxed tuple here but we really want GHC to optimize it away.
--
decodeChar :: PrimArray Word8 -> Int -> (# Char, Int #)
{-# INLINE decodeChar #-}
decodeChar (PrimArray (ByteArray ba#)) (I# idx#) =
    let (# c#, i# #) = decodeChar# ba# idx# in (# C# c#, I# i# #)

-- | The unboxed version of 'decodeChar'
--
-- This function is marked as @NOINLINE@ to reduce code size.
--
decodeChar# :: ByteArray# -> Int# -> (# Char#, Int# #)
{-# NOINLINE decodeChar# #-} -- This branchy code make GHC impossible to fuse, DON'T inline
decodeChar# ba# idx# = case indexWord8Array# ba# idx# of
    w1#
        | isTrue# (w1# `leWord#` 0x7F##) -> (# chr1# w1#, 1# #)
        | isTrue# (w1# `leWord#` 0xDF##) ->
            let w2# = indexWord8Array# ba# (idx# +# 1#)
            in (# chr2# w1# w2#, 2# #)
        | isTrue# (w1# `leWord#` 0xEF##) ->
            let w2# = indexWord8Array# ba# (idx# +# 1#)
                w3# = indexWord8Array# ba# (idx# +# 2#)
            in (# chr3# w1# w2# w3#, 3# #)
        | otherwise ->
            let w2# = indexWord8Array# ba# (idx# +# 1#)
                w3# = indexWord8Array# ba# (idx# +# 2#)
                w4# = indexWord8Array# ba# (idx# +# 3#)
            in (# chr4# w1# w2# w3# w4#, 4# #)

-- | Decode a codepoint's length in bytes
--
-- This function assumed all bytes are UTF-8 encoded, and the index param point to the
-- beginning of a codepoint.
--
decodeCharLen :: PrimArray Word8 -> Int -> Int
{-# INLINE decodeCharLen #-}
decodeCharLen (PrimArray (ByteArray ba#)) (I# idx#) =
    let i# = decodeCharLen# ba# idx# in I# i#

-- | The unboxed version of 'decodeCharLen'
--
-- This function is marked as @NOINLINE@ to reduce code size.
--
decodeCharLen# :: ByteArray# -> Int# -> Int#
{-# INLINE decodeCharLen# #-} -- This branchy code make GHC impossible to fuse, DON'T inline
decodeCharLen# ba# idx# = case indexWord8Array# ba# idx# of
    w1#
        | isTrue# (w1# `leWord#` 0x7F##) -> 1#
        | isTrue# (w1# `leWord#` 0xDF##) -> 2#
        | isTrue# (w1# `leWord#` 0xEF##) -> 3#
        | otherwise -> 4#

-- | Decode a 'Char' from bytes in rerverse order.
--
-- This function assumed all bytes are UTF-8 encoded, and the index param point to the end
-- of a codepoint, the decoded character and the backward advancing offset are returned.
--
decodeCharReverse :: PrimArray Word8 -> Int -> (# Char, Int #)
{-# INLINE decodeCharReverse #-}
decodeCharReverse (PrimArray (ByteArray ba#)) (I# idx#) =
    let (# c#, i# #) = decodeCharReverse# ba# idx# in (# C# c#, I# i# #)

-- | The unboxed version of 'decodeCharReverse'
--
-- This function is marked as @NOINLINE@ to reduce code size.
--
decodeCharReverse# :: ByteArray# -> Int# -> (# Char#, Int# #)
{-# NOINLINE decodeCharReverse# #-} -- This branchy code make GHC impossible to fuse, DON'T inline
decodeCharReverse# ba# idx# =
    let w1# = indexWord8Array# ba# idx#
    in if isContinueByte# w1#
    then
        let w2# = indexWord8Array# ba# (idx# -# 1#)
        in if isContinueByte# w2#
        then
            let w3# = indexWord8Array# ba# (idx# -# 2#)
            in if isContinueByte# w3#
            then
                let w4# = indexWord8Array# ba# (idx# -# 3#)
                in (# chr4# w4# w3# w2# w1#, 4# #)
            else (# chr3# w3# w2# w1#, 3# #)
        else  (# chr2# w2# w1#, 2# #)
    else (# chr1# w1#, 1# #)


-- | Decode a codepoint's length in bytes in reverse order.
--
-- This function assumed all bytes are UTF-8 encoded, and the index param point to the end
-- of a codepoint.
--
decodeCharLenReverse :: PrimArray Word8 -> Int -> Int
{-# INLINE decodeCharLenReverse #-}
decodeCharLenReverse (PrimArray (ByteArray ba#)) (I# idx#) =
    let i# = decodeCharLenReverse# ba# idx# in I# i#

-- | The unboxed version of 'decodeCharLenReverse'
--
-- This function is marked as @NOINLINE@ to reduce code size.
--
decodeCharLenReverse# :: ByteArray# -> Int# -> Int#
{-# NOINLINE decodeCharLenReverse# #-} -- This branchy code make GHC impossible to fuse, DON'T inline
decodeCharLenReverse# ba# idx# =
    let w1# = indexWord8Array# ba# idx#
    in if isContinueByte# w1#
    then
        let w2# = indexWord8Array# ba# (idx# -# 1#)
        in if isContinueByte# w2#
        then
            let w3# = indexWord8Array# ba# (idx# -# 2#)
            in if isContinueByte# w3#
            then 4#
            else 3#
        else 2#
    else 1#

-- | Validate if current index point to a valid utf8 codepoint.
--
-- If the codepoint is valid, return the utf8 bytes length, otherwise return the negation of
-- offset we should skip(so that a replacing decoder can meet the security rules).
-- If @0#@ is returned, then you should feed more bytes to continue validation. Note this function
-- guarantee only return @0@ when the trailing bytes are partial valid codepoint. That means you
-- can safely replace them with a replacement char or ignore it if no more bytes are to be fed.
--
-- reference: https://howardhinnant.github.io/utf_summary.html
--
validateChar :: PrimArray Word8
             -> Int  -- current index
             -> Int  -- end index(which shouldn't be accessed)
             -> Int
{-# INLINE validateChar #-}
validateChar (PrimArray (ByteArray ba#)) (I# idx#) (I# end#) =
    let i# = validateChar# ba# idx# end# in I# i#


-- | The unboxed version of 'decodeCharLenReverse'
--
-- This function is marked as @NOINLINE@ to reduce code size.
--
validateChar# :: ByteArray# -> Int# -> Int# -> Int#
{-# NOINLINE validateChar# #-}
validateChar# ba# idx# end# =
    let w1# = indexWord8Array# ba# idx#
    in case end# -# idx# of
        1#
            | isTrue# (w1# `leWord#` 0x7F##) -> 1#
            | isTrue# (w1# `leWord#` 0xC1##) -> -1#
            | isTrue# (w1# `geWord#` 0xF5##) -> -1#
            | otherwise -> 0#
        2#
            | isTrue# (w1# `leWord#` 0x7F##) -> 1#
            | isTrue# (w1# `leWord#` 0xC1##) -> -1#
            | isTrue# (w1# `leWord#` 0xDF##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                in if between# w2# 0x80## 0xBF##
                then 2#
                else -1#
            | isTrue# (w1# `geWord#` 0xF5##) -> -1#
            | otherwise -> 0#
        3#
            | isTrue# (w1# `leWord#` 0x7F##) -> 1#
            | isTrue# (w1# `leWord#` 0xC1##) -> -1#
            | isTrue# (w1# `leWord#` 0xDF##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                in if between# w2# 0x80## 0xBF##
                then 2#
                else -1#

            | isTrue# (w1# `eqWord#` 0xE0##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0xA0## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#

            | isTrue# (w1# `leWord#` 0xEC##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0x80## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#

            | isTrue# (w1# `eqWord#` 0xED##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0x80## 0x9F##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#
            | isTrue# (w1# `eqWord#` 0xEF##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0x80## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#
            | isTrue# (w1# `geWord#` 0xF5##) -> -1#
            | otherwise -> 0#

        _
            | isTrue# (w1# `leWord#` 0x7F##) -> 1#
            | isTrue# (w1# `leWord#` 0xC1##) -> -1#
            | isTrue# (w1# `leWord#` 0xDF##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                in if between# w2# 0x80## 0xBF##
                then 2#
                else -1#

            | isTrue# (w1# `eqWord#` 0xE0##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0xA0## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#

            | isTrue# (w1# `leWord#` 0xEC##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0x80## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#

            | isTrue# (w1# `eqWord#` 0xED##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0x80## 0x9F##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#
            | isTrue# (w1# `eqWord#` 0xEF##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0x80## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#

            | isTrue# (w1# `eqWord#` 0xF0##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                    w4# = indexWord8Array# ba# (idx# +# 3#)
                in if between# w2# 0x90## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then if between# w4# 0x80## 0xBF##
                        then 4#
                        else -3#
                    else -2#
                else -1#

            | isTrue# (w1# `leWord#` 0xF3##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                    w4# = indexWord8Array# ba# (idx# +# 3#)
                in if between# w2# 0x80## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then if between# w4# 0x80## 0xBF##
                        then 4#
                        else -3#
                    else -2#
                else -1#

            | isTrue# (w1# `eqWord#` 0xF4##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                    w4# = indexWord8Array# ba# (idx# +# 3#)
                in if between# w2# 0x80## 0x8F##
                then if between# w3# 0x80## 0xBF##
                    then if between# w4# 0x80## 0xBF##
                        then 4#
                        else -3#
                    else -2#
                else -1#
            | isTrue# (w1# `geWord#` 0xF5##) -> -1#
            | otherwise -> -1#

between# :: Word# -> Word# -> Word# -> Bool
{-# INLINE between# #-}
between# w# l# h# = isTrue# (w# `geWord#` l#) && isTrue# (w# `leWord#` h#)

isContinueByte# :: Word# -> Bool
{-# INLINE isContinueByte# #-}
isContinueByte# w# = isTrue# (and# w# 0xC0## `eqWord#` 0x80##)

chr1# :: Word# -> Char#
{-# INLINE chr1# #-}
chr1# x1# = chr# y1#
  where
    !y1# = word2Int# x1#

chr2# :: Word# -> Word# -> Char#
{-# INLINE chr2# #-}
chr2# x1# x2# = chr# (z1# +# z2#)
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
    !z2# = y2# -# 0x80#

chr3# :: Word# -> Word# -> Word# -> Char#
{-# INLINE chr3# #-}
chr3# x1# x2# x3# = chr# (z1# +# z2# +# z3#)
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !y3# = word2Int# x3#
    !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
    !z3# = y3# -# 0x80#

chr4# :: Word# -> Word# -> Word# -> Word# -> Char#
{-# INLINE chr4# #-}
chr4# x1# x2# x3# x4# = chr# (z1# +# z2# +# z3# +# z4#)
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !y3# = word2Int# x3#
    !y4# = word2Int# x4#
    !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
    !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
    !z4# = y4# -# 0x80#

-- | Unrolled copy loop for copying a utf8-encoded codepoint from source array to target array.
--
copyChar :: Int                       -- copy length, must be 1, 2, 3 or 4
         -> MutablePrimArray s Word8  -- target array
         -> Int                       -- writing offset
         -> PrimArray Word8           -- source array
         -> Int                       -- reading offset
         -> ST s ()
{-# INLINE copyChar #-}
copyChar !l !mba !j !ba !i = case l of
    1 -> do writePrimArray mba j $ indexPrimArray ba i
    2 -> do writePrimArray mba j $ indexPrimArray ba i
            writePrimArray mba (j+1) $ indexPrimArray ba (i+1)
    3 -> do writePrimArray mba j $ indexPrimArray ba i
            writePrimArray mba (j+1) $ indexPrimArray ba (i+1)
            writePrimArray mba (j+2) $ indexPrimArray ba (i+2)
    _ -> do writePrimArray mba j $ indexPrimArray ba i
            writePrimArray mba (j+1) $ indexPrimArray ba (i+1)
            writePrimArray mba (j+2) $ indexPrimArray ba (i+2)
            writePrimArray mba (j+3) $ indexPrimArray ba (i+3)
