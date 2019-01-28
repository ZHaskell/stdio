module Std.Data.Parser.Numeric where

import Std.Data.Vector.Base     as V
import Std.Data.Vector.Extra    as V


readSigned
    :: (Num a)
    => (ByteString -> Maybe (a, ByteString))
    ->  ByteString -> Maybe (a, ByteString)
readSigned f xs
    | BS.null xs = Nothing
    | otherwise  =
        case BSU.unsafeHead xs of
        0x2D -> f (BSU.unsafeTail xs) >>= \(n, ys) -> return (negate n, ys)
        0x2B -> f (BSU.unsafeTail xs)
        _    -> f xs

----------------------------------------------------------------
----- Decimal

{-
-- We unroll this definition in order to reduce the number of conversions from native Int to the Integral type.
readDecimalSimple :: (Integral a) => ByteString -> Maybe (a, ByteString)
readDecimalSimple = start
    where
    -- This implementation is near verbatim from
    -- bytestring-0.9.1.7:Data.ByteString.Char8.readInt. We do
    -- remove the superstrictness by lifting the 'Just' so it can
    -- be returned after seeing the first byte. Do beware of the
    -- scope of 'fromIntegral', we want to avoid unnecessary
    -- 'Integral' operations and do as much as possible in 'Word8'.
    start xs
        | BS.null xs = Nothing
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    Just $ loop (fromIntegral (w - 0x30)) (BSU.unsafeTail xs)
              | otherwise -> Nothing

    loop !n !xs
        | BS.null xs = (n, BS.empty) -- not @xs@, to help GC
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop (n * 10 + fromIntegral (w - 0x30)) (BSU.unsafeTail xs)
              | otherwise -> (n,xs)
-}

-- | Read an unsigned\/non-negative integral value in ASCII decimal
-- format. Returns @Nothing@ if there is no integer at the beginning
-- of the string, otherwise returns @Just@ the integer read and the
-- remainder of the string.
--
-- If you are extremely concerned with performance, then it is more
-- performant to use this function at @Int@ or @Word@ and then to
-- call 'fromIntegral' to perform the conversion at the end. However,
-- doing this will make your code succeptible to overflow bugs if
-- the target type is larger than @Int@.
readDecimal :: (Integral a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readDecimal ::
    ByteString -> Maybe (Int,     ByteString),
    ByteString -> Maybe (Int8,    ByteString),
    ByteString -> Maybe (Int16,   ByteString),
    ByteString -> Maybe (Int32,   ByteString),
    ByteString -> Maybe (Int64,   ByteString),
    ByteString -> Maybe (Integer, ByteString),
    ByteString -> Maybe (Word,    ByteString),
    ByteString -> Maybe (Word8,   ByteString),
    ByteString -> Maybe (Word16,  ByteString),
    ByteString -> Maybe (Word32,  ByteString),
    ByteString -> Maybe (Word64,  ByteString) #-}
readDecimal = start
    where
    -- TODO: should we explicitly drop all leading zeros before we jump into the unrolled loop?
    start :: (Integral a) => ByteString -> Maybe (a, ByteString)
    start xs
        | BS.null xs = Nothing
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> Just $ loop0 (toDigit w) (BSU.unsafeTail xs)
              | otherwise   -> Nothing

    loop0 :: (Integral a) => a -> ByteString -> (a, ByteString)
    loop0 !m !xs
        | BS.null xs = (m, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop1 m (toDigit w) (BSU.unsafeTail xs)
              | otherwise   -> (m, xs)

    loop1, loop2, loop3, loop4, loop5, loop6, loop7, loop8
        :: (Integral a) => a -> Int -> ByteString -> (a, ByteString)
    loop1 !m !n !xs
        | BS.null xs = (m*10 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop2 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*10 + fromIntegral n, xs)
    loop2 !m !n !xs
        | BS.null xs = (m*100 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop3 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*100 + fromIntegral n, xs)
    loop3 !m !n !xs
        | BS.null xs = (m*1000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop4 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*1000 + fromIntegral n, xs)
    loop4 !m !n !xs
        | BS.null xs = (m*10000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop5 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*10000 + fromIntegral n, xs)
    loop5 !m !n !xs
        | BS.null xs = (m*100000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop6 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*100000 + fromIntegral n, xs)
    loop6 !m !n !xs
        | BS.null xs = (m*1000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop7 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*1000000 + fromIntegral n, xs)
    loop7 !m !n !xs
        | BS.null xs = (m*10000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop8 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*10000000 + fromIntegral n, xs)
    loop8 !m !n !xs
        | BS.null xs = (m*100000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop0
                    (m*1000000000 + fromIntegral (addDigit n w))
                    (BSU.unsafeTail xs)
              | otherwise   -> (m*100000000 + fromIntegral n, xs)

----------------------------------------------------------------

-- | A variant of 'readDecimal' which does not return the tail of
-- the string, and returns @0@ instead of @Nothing@. This is twice
-- as fast for 'Int64' on 32-bit systems, but has identical performance
-- to 'readDecimal' for all other types and architectures.
--
-- /Since: 0.4.0/
readDecimal_ :: (Integral a) => ByteString -> a
{-# SPECIALIZE readDecimal_ ::
    ByteString -> Int,
    ByteString -> Int8,
    ByteString -> Int16,
    ByteString -> Int32,
    ByteString -> Int64,
    ByteString -> Integer,
    ByteString -> Word,
    ByteString -> Word8,
    ByteString -> Word16,
    ByteString -> Word32,
    ByteString -> Word64 #-}
readDecimal_ = start
    where
    start xs
        | BS.null xs = 0
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop0 (toDigit w) (BSU.unsafeTail xs)
              | otherwise   -> 0

    loop0 :: (Integral a) => a -> ByteString -> a
    loop0 !m !xs
        | BS.null xs = m
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop1 m (toDigit w) (BSU.unsafeTail xs)
              | otherwise   -> m

    loop1, loop2, loop3, loop4, loop5, loop6, loop7, loop8
        :: (Integral a) => a -> Int -> ByteString -> a
    loop1 !m !n !xs
        | BS.null xs = m*10 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop2 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*10 + fromIntegral n
    loop2 !m !n !xs
        | BS.null xs = m*100 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop3 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*100 + fromIntegral n
    loop3 !m !n !xs
        | BS.null xs = m*1000 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop4 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*1000 + fromIntegral n
    loop4 !m !n !xs
        | BS.null xs = m*10000 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop5 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*10000 + fromIntegral n
    loop5 !m !n !xs
        | BS.null xs = m*100000 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop6 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*100000 + fromIntegral n
    loop6 !m !n !xs
        | BS.null xs = m*1000000 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop7 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*1000000 + fromIntegral n
    loop7 !m !n !xs
        | BS.null xs = m*10000000 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop8 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*10000000 + fromIntegral n
    loop8 !m !n !xs
        | BS.null xs = m*100000000 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop0
                    (m*1000000000 + fromIntegral (addDigit n w))
                    (BSU.unsafeTail xs)
              | otherwise   -> m*100000000 + fromIntegral n

----------------------------------------------------------------
-- | Convert a non-negative integer into an (unsigned) ASCII decimal
-- string. Returns @Nothing@ on negative inputs.
packDecimal :: (Integral a) => a -> Maybe ByteString
{-# INLINE packDecimal #-}
packDecimal n
    | n < 0     = Nothing
    | otherwise = Just (unsafePackDecimal n)


-- This implementation is modified from:
-- <http://www.serpentine.com/blog/2013/03/20/whats-good-for-c-is-good-for-haskell/>
-- See the banchmarks for implementation details.
-- BUG: the additional guard in 'numDecimalDigits' results in a 3x slowdown!!
--
-- | Convert a non-negative integer into an (unsigned) ASCII decimal
-- string. This function is unsafe to use on negative inputs.
unsafePackDecimal :: (Integral a) => a -> ByteString
{-# SPECIALIZE unsafePackDecimal ::
    Int     -> ByteString,
    Int8    -> ByteString,
    Int16   -> ByteString,
    Int32   -> ByteString,
    Int64   -> ByteString,
    Integer -> ByteString,
    Word    -> ByteString,
    Word8   -> ByteString,
    Word16  -> ByteString,
    Word32  -> ByteString,
    Word64  -> ByteString #-}
unsafePackDecimal n0 =
    let size = numDecimalDigits n0
    in  BSI.unsafeCreate size $ \p0 -> loop n0 (p0 `plusPtr` (size - 1))
    where
    getDigit = BSU.unsafeIndex packDecimal_digits

    loop !n !p
        | n >= 100  = do
            let (q,r) = n `quotRem` 100
            write2 r p
            loop   q (p `plusPtr` negate 2)
        | n >= 10   = write2 n p
        | otherwise = poke p (0x30 + fromIntegral n)

    write2 !i0 !p = do
        let i = fromIntegral i0; j = i + i
        poke p                      (getDigit $! j + 1)
        poke (p `plusPtr` negate 1) (getDigit j)

packDecimal_digits :: ByteString
{-# NOINLINE packDecimal_digits #-}
packDecimal_digits = BS8.pack
    "0001020304050607080910111213141516171819\
    \2021222324252627282930313233343536373839\
    \4041424344454647484950515253545556575859\
    \6061626364656667686970717273747576777879\
    \8081828384858687888990919293949596979899"
    -- BUG: jEdit syntax highlighting fail: ->

----------------------------------------------------------------
----------------------------------------------------------------
----- Hexadecimal

-- | Read a non-negative integral value in ASCII hexadecimal format.
-- Returns @Nothing@ if there is no integer at the beginning of the
-- string, otherwise returns @Just@ the integer read and the remainder
-- of the string.
--
-- This function does not recognize the various hexadecimal sigils
-- like \"0x\", but because there are so many different variants,
-- those are best handled by helper functions which then use this
-- function for the actual numerical parsing. This function recognizes
-- both upper-case, lower-case, and mixed-case hexadecimal.
readHexadecimal :: (Integral a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readHexadecimal ::
    ByteString -> Maybe (Int,     ByteString),
    ByteString -> Maybe (Int8,    ByteString),
    ByteString -> Maybe (Int16,   ByteString),
    ByteString -> Maybe (Int32,   ByteString),
    ByteString -> Maybe (Int64,   ByteString),
    ByteString -> Maybe (Integer, ByteString),
    ByteString -> Maybe (Word,    ByteString),
    ByteString -> Maybe (Word8,   ByteString),
    ByteString -> Maybe (Word16,  ByteString),
    ByteString -> Maybe (Word32,  ByteString),
    ByteString -> Maybe (Word64,  ByteString) #-}
readHexadecimal = start
    where
    -- TODO: Would it be worth trying to do the magichash trick
    -- used by Warp here? It'd really help remove branch prediction
    -- issues etc.
    --
    -- Beware the urge to make this code prettier, cf 'readDecimal'.
    start xs
        | BS.null xs = Nothing
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    Just $ loop (fromIntegral (w - 0x30))  (BSU.unsafeTail xs)
              | 0x46 >= w && w >= 0x41 ->
                    Just $ loop (fromIntegral (w-0x41+10)) (BSU.unsafeTail xs)
              | 0x66 >= w && w >= 0x61 ->
                    Just $ loop (fromIntegral (w-0x61+10)) (BSU.unsafeTail xs)
              | otherwise -> Nothing

    loop !n !xs
        | BS.null xs = (n, BS.empty) -- not @xs@, to help GC
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop (n*16 + fromIntegral (w - 0x30))  (BSU.unsafeTail xs)
              | 0x46 >= w && w >= 0x41 ->
                    loop (n*16 + fromIntegral (w-0x41+10)) (BSU.unsafeTail xs)
              | 0x66 >= w && w >= 0x61 ->
                    loop (n*16 + fromIntegral (w-0x61+10)) (BSU.unsafeTail xs)
              | otherwise -> (n,xs)


-- | Convert a non-negative integer into a lower-case ASCII hexadecimal
-- string. Returns @Nothing@ on negative inputs.
packHexadecimal :: (Integral a) => a -> Maybe ByteString
{-# INLINE packHexadecimal #-}
packHexadecimal n
    | n < 0     = Nothing
    | otherwise = Just (unsafePackHexadecimal n)


-- | Convert a non-negative integer into a lower-case ASCII hexadecimal
-- string. This function is unsafe to use on negative inputs.
unsafePackHexadecimal :: (Integral a) => a -> ByteString
{-# SPECIALIZE unsafePackHexadecimal ::
    Int     -> ByteString,
    Int8    -> ByteString,
    Int16   -> ByteString,
    Int32   -> ByteString,
    Int64   -> ByteString,
    Integer -> ByteString,
    Word    -> ByteString,
    Word8   -> ByteString,
    Word16  -> ByteString,
    Word32  -> ByteString,
    Word64  -> ByteString #-}
unsafePackHexadecimal n0 =
    let size = numTwoPowerDigits 4 (toInteger n0) -- for Bits
    in  BSI.unsafeCreate size $ \p0 ->
            loop n0 (p0 `plusPtr` (size - 1))
    where
    -- TODO: benchmark using @hexDigits@ vs using direct manipulations.
    loop :: (Integral a) => a -> Ptr Word8 -> IO ()
    loop n p
        | n <= 15   = do
            poke p (BSU.unsafeIndex hexDigits (fromIntegral n .&. 0x0F))
        | otherwise = do
            let (q,r) = n `quotRem` 16
            poke p (BSU.unsafeIndex hexDigits (fromIntegral r .&. 0x0F))
            loop q (p `plusPtr` negate 1)


-- Inspired by, <http://forums.xkcd.com/viewtopic.php?f=11&t=16666&p=553936>
-- | Convert a bitvector into a lower-case ASCII hexadecimal string.
-- This is helpful for visualizing raw binary data, rather than for
-- parsing as such.
asHexadecimal :: ByteString -> ByteString
asHexadecimal = start
    where
    start buf
        | BS.length buf > maxBound `quot` 2 =
            error _asHexadecimal_overflow
        | otherwise =
            BSI.unsafeCreate (2 * BS.length buf) $ \p0 -> do
                _ <- foldIO step p0 buf
                return () -- needed for type checking

    step :: Ptr Word8 -> Word8 -> IO (Ptr Word8)
    step !p !w = do
        let ix = fromIntegral w
        poke   p     (BSU.unsafeIndex hexDigits ((ix .&. 0xF0) `shiftR` 4))
        poke   (p `plusPtr` 1) (BSU.unsafeIndex hexDigits  (ix .&. 0x0F))
        return (p `plusPtr` 2)

_asHexadecimal_overflow :: String
{-# NOINLINE _asHexadecimal_overflow #-}
_asHexadecimal_overflow =
    "asHexadecimal: cannot create buffer larger than (maxBound::Int)"


-- TODO: benchmark against the magichash hack used in Warp.
-- | The lower-case ASCII hexadecimal digits, in numerical order
-- for use as a lookup table.
hexDigits :: ByteString
{-# NOINLINE hexDigits #-}
hexDigits = BS8.pack "0123456789abcdef"


-- | We can only do this for MonadIO not just any Monad, but that's
-- good enough for what we need...
foldIO :: (a -> Word8 -> IO a) -> a -> ByteString -> IO a
{-# INLINE foldIO #-}
foldIO f z0 (BSI.PS fp off len) =
    FFI.withForeignPtr fp $ \p0 -> do
        let q = p0 `plusPtr` (off+len)
        let go !z !p
                | p == q    = return z
                | otherwise = do
                    w  <- peek p
                    z' <- f z w
                    go z' (p `plusPtr` 1)
        go z0 (p0 `plusPtr` off)


----------------------------------------------------------------
----------------------------------------------------------------
----- Octal

-- | Read a non-negative integral value in ASCII octal format.
-- Returns @Nothing@ if there is no integer at the beginning of the
-- string, otherwise returns @Just@ the integer read and the remainder
-- of the string.
--
-- This function does not recognize the various octal sigils like
-- \"0o\", but because there are different variants, those are best
-- handled by helper functions which then use this function for the
-- actual numerical parsing.
readOctal :: (Integral a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readOctal ::
    ByteString -> Maybe (Int,     ByteString),
    ByteString -> Maybe (Int8,    ByteString),
    ByteString -> Maybe (Int16,   ByteString),
    ByteString -> Maybe (Int32,   ByteString),
    ByteString -> Maybe (Int64,   ByteString),
    ByteString -> Maybe (Integer, ByteString),
    ByteString -> Maybe (Word,    ByteString),
    ByteString -> Maybe (Word8,   ByteString),
    ByteString -> Maybe (Word16,  ByteString),
    ByteString -> Maybe (Word32,  ByteString),
    ByteString -> Maybe (Word64,  ByteString) #-}
readOctal = start
    where
    start xs
        | BS.null xs = Nothing
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x37 >= w && w >= 0x30 ->
                    Just $ loop (fromIntegral (w - 0x30)) (BSU.unsafeTail xs)
              | otherwise -> Nothing

    loop !n !xs
        | BS.null xs = (n, BS.empty) -- not @xs@, to help GC
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x37 >= w && w >= 0x30 ->
                    loop (n * 8 + fromIntegral (w - 0x30)) (BSU.unsafeTail xs)
              | otherwise -> (n,xs)


-- | Convert a non-negative integer into an ASCII octal string.
-- Returns @Nothing@ on negative inputs.
packOctal :: (Integral a) => a -> Maybe ByteString
{-# INLINE packOctal #-}
packOctal n
    | n < 0     = Nothing
    | otherwise = Just (unsafePackOctal n)


-- | Convert a non-negative integer into an ASCII octal string.
-- This function is unsafe to use on negative inputs.
unsafePackOctal :: (Integral a) => a -> ByteString
{-# SPECIALIZE unsafePackOctal ::
    Int     -> ByteString,
    Int8    -> ByteString,
    Int16   -> ByteString,
    Int32   -> ByteString,
    Int64   -> ByteString,
    Integer -> ByteString,
    Word    -> ByteString,
    Word8   -> ByteString,
    Word16  -> ByteString,
    Word32  -> ByteString,
    Word64  -> ByteString #-}
unsafePackOctal n0 =
    let size = numTwoPowerDigits 3 (toInteger n0) -- for Bits
    in  BSI.unsafeCreate size $ \p0 ->
            loop n0 (p0 `plusPtr` (size - 1))
    where
    loop :: (Integral a) => a -> Ptr Word8 -> IO ()
    loop n p
        | n <= 7    = do
            poke p (0x30 + fromIntegral n)
        | otherwise = do
            let (q,r) = n `quotRem` 8
            poke p (0x30 + fromIntegral r)
            loop q (p `plusPtr` negate 1)

{-
-- BUG: This doesn't quite work right...
asOctal :: ByteString -> ByteString
asOctal buf =
    BSI.unsafeCreate (ceilEightThirds $ BS.length buf) $ \p0 -> do
        let (BSI.PS fq off len) = buf
        FFI.withForeignPtr fq $ \q0 -> do
            let qF = q0 `plusPtr` (off + len - rem len 3)
            let loop :: Ptr Word8 -> Ptr Word8 -> IO ()
                loop p q
                    | q /= qF   = do
                        {- Take three Word8s and write 8 chars at a time -}
                        i <- peek q
                        j <- peek (q `plusPtr` 1) :: IO Word8
                        k <- peek (q `plusPtr` 2) :: IO Word8
                        let w =     fromIntegral i
                                .|. (fromIntegral j `shiftL` 8)
                                .|. (fromIntegral k `shiftL` 16)
                        poke p               (toC8( w              .&. 0x07))
                        poke (p `plusPtr` 1) (toC8((w `shiftR`  3) .&. 0x07))
                        poke (p `plusPtr` 2) (toC8((w `shiftR`  6) .&. 0x07))
                        poke (p `plusPtr` 3) (toC8((w `shiftR`  9) .&. 0x07))
                        poke (p `plusPtr` 4) (toC8((w `shiftR` 12) .&. 0x07))
                        poke (p `plusPtr` 5) (toC8((w `shiftR` 15) .&. 0x07))
                        poke (p `plusPtr` 6) (toC8((w `shiftR` 18) .&. 0x07))
                        poke (p `plusPtr` 7) (toC8((w `shiftR` 21) .&. 0x07))
                        loop (p `plusPtr` 8) (q `plusPtr` 3)
                    | 2 == rem len 3 = do
                        {- Handle the last two Word8s -}
                        i <- peek q
                        j <- peek (q `plusPtr` 1) :: IO Word8
                        let w =      fromIntegral i
                                .|. (fromIntegral j `shiftL` 8)
                        poke p               (toC8( w              .&. 0x07))
                        poke (p `plusPtr` 1) (toC8((w `shiftR`  3) .&. 0x07))
                        poke (p `plusPtr` 2) (toC8((w `shiftR`  6) .&. 0x07))
                        poke (p `plusPtr` 3) (toC8((w `shiftR`  9) .&. 0x07))
                        poke (p `plusPtr` 4) (toC8((w `shiftR` 12) .&. 0x07))

----------------------------------------------------------------
----------------------------------------------------------------
----- Character-based bit-bashing

{-# INLINE isNotPeriod #-}
isNotPeriod :: Word8 -> Bool
isNotPeriod w = w /= 0x2E

{-# INLINE isNotE #-}
isNotE :: Word8 -> Bool
isNotE w = w /= 0x65 && w /= 0x45

{-# INLINE isDecimal #-}
isDecimal :: Word8 -> Bool
isDecimal w = 0x39 >= w && w >= 0x30

{-# INLINE isDecimalZero #-}
isDecimalZero :: Word8 -> Bool
isDecimalZero w = w == 0x30

{-# INLINE toDigit #-}
toDigit :: (Integral a) => Word8 -> a
toDigit w = fromIntegral (w - 0x30)

{-# INLINE addDigit #-}
addDigit :: Int -> Word8 -> Int
addDigit n w = n * 10 + toDigit w
