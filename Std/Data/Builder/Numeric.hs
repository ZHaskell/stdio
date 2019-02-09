{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE UnliftedFFITypes    #-}

{-|
Module      : Std.Data.Builder.Numeric
Description : Textual numeric builders.
Copyright   : (c) Winterland, 2017-2019
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

Textual numeric builders.

-}

module Std.Data.Builder.Numeric (
  -- * Integral type formatting
    IFormat(..)
  , defaultIFormat
  , Padding(..)
  , int
  , intWith
  , integer
  -- * Fixded size hexidecimal formatting
  , hex, heX
  -- * IEEE float formating
  , FFormat(..)
  , double
  , doubleWith
  , float
  , floatWith
  , scientific
  , scientificWith
  -- * Misc
  , grisu3
  , grisu3_sp
  , i2wDec, i2wHex, i2wHeX
  , countDigits
) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.Char
import           Data.Int
import qualified Data.List                           as List
import           Data.Primitive.Addr
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import qualified Data.Scientific                     as Sci
import           Data.Word
import           GHC.Exts
import           GHC.Float
import           GHC.Integer
import           GHC.Types
import           Std.Data.Builder.Base
import           Std.Data.Builder.Numeric.DigitTable
import           Std.Data.Text.Base
import           Std.Foreign.PrimArray
import           System.IO.Unsafe
#ifdef INTEGER_GMP
import           GHC.Integer.GMP.Internals
#endif
import           GHC.Float                           (roundTo)

--------------------------------------------------------------------------------

-- | Integral formatting options.
--
data IFormat = IFormat
    { width       :: Int              -- ^ total width, only effective with padding options
    , padding     :: Padding        -- ^ padding options
    , postiveSign :: Bool       -- ^ show @+@ when the number is positive
    } deriving (Show, Eq, Ord)

-- | @defaultIFormat = IFormat 0 NoPadding False Decimal@
defaultIFormat :: IFormat
defaultIFormat = IFormat 0 NoPadding False

data Padding = NoPadding | ZeroPadding | LeftSpacePadding | RightSpacePadding deriving (Show, Eq, Ord)

-- | @int = intWith defaultIFormat@
int :: (Integral a, Bounded a) => a -> Builder ()
int = intWith defaultIFormat

-- | Format a 'Bounded' 'Integral' type like @Int@ or @Word16@ into decimal ASCII digits.
intWith :: (Integral a, Bounded a)
        => IFormat
        -> a
        -> Builder ()
{-# INLINE[1] intWith #-}
{-# RULES "intWith'/Int8"    intWith = intWith' :: IFormat -> Int8    -> Builder () #-}
{-# RULES "intWith'/Int"     intWith = intWith' :: IFormat -> Int     -> Builder () #-}
{-# RULES "intWith'/Int16"   intWith = intWith' :: IFormat -> Int16   -> Builder () #-}
{-# RULES "intWith'/Int32"   intWith = intWith' :: IFormat -> Int32   -> Builder () #-}
{-# RULES "intWith'/Int64"   intWith = intWith' :: IFormat -> Int64   -> Builder () #-}
{-# RULES "intWith'/Word"    intWith = positiveInt  :: IFormat -> Word    -> Builder () #-}
{-# RULES "intWith'/Word8"   intWith = positiveInt  :: IFormat -> Word8   -> Builder () #-}
{-# RULES "intWith'/Word16"  intWith = positiveInt  :: IFormat -> Word16  -> Builder () #-}
{-# RULES "intWith'/Word32"  intWith = positiveInt  :: IFormat -> Word32  -> Builder () #-}
{-# RULES "intWith'/Word64"  intWith = positiveInt  :: IFormat -> Word64  -> Builder () #-}
intWith = intWith'

intWith' :: (Integral a, Bounded a) => IFormat -> a -> Builder ()
{-# SPECIALIZE INLINE intWith' :: IFormat -> Int   -> Builder () #-}
{-# SPECIALIZE INLINE intWith' :: IFormat -> Int8  -> Builder () #-}
{-# SPECIALIZE INLINE intWith' :: IFormat -> Int16 -> Builder () #-}
{-# SPECIALIZE INLINE intWith' :: IFormat -> Int32 -> Builder () #-}
{-# SPECIALIZE INLINE intWith' :: IFormat -> Int64 -> Builder () #-}
intWith' format@(IFormat width padding _) i
    | i < 0 =
        if i == minBound            -- can't directly negate in this case
        then do
            let (q, r) = i `quotRem` 10
                !qq = -q            -- all digits except last one
                !rr = i2wDec (-r)      -- last digits
                !n = countDigits qq
                !n' = n + 2         -- extra two bytes: minus and last digit
            if width > n'
            then case padding of
                NoPadding ->
                    writeN n' $ \marr off -> do
                        writePrimArray marr off minus                       -- leading minus
                        let off' = off + 1
                        writePositiveDec marr off' n qq                      -- digits
                        let off'' = off' + n
                        writePrimArray marr off'' rr                        -- last digit
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        writePrimArray marr off minus                   -- leading minus
                        let off' = off + 1
                        setPrimArray marr off' leadingN zero            -- leading zeros
                        let off'' = off' + leadingN
                        writePositiveDec marr off'' n qq                 -- digits
                        let off''' = off'' + n
                        writePrimArray marr off''' rr                   -- last digit
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        setPrimArray marr off leadingN space            -- leading spaces
                        let off' = off + leadingN
                        writePrimArray marr off' minus                  -- leading minus
                        let off'' = off' + 1
                        writePositiveDec marr off'' n qq                 -- digits
                        let off''' = off'' + n
                        writePrimArray marr off''' rr                   -- last digit
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n'
                        writePrimArray marr off minus                   -- leading minus
                        let off' = off + 1
                        writePositiveDec marr off' n qq                  -- digits
                        let off'' = off' + n
                        writePrimArray marr off'' rr                    -- last digit
                        let off''' = off'' + 1
                        setPrimArray marr off''' trailingN space        -- trailing spaces
            else
                writeN n' $ \marr off -> do
                    writePrimArray marr off minus                       -- leading minus
                    let off' = off + 1
                    writePositiveDec marr off' n qq                      -- digits
                    let off'' = off' + n
                    writePrimArray marr off'' rr                        -- last digit
        else do
            let !qq = -i
                !n = countDigits qq
                !n' = n + 1  -- extra byte: minus
            if width > n'
            then case padding of
                NoPadding ->
                    writeN n' $ \marr off -> do
                        writePrimArray marr off minus                       -- leading minus
                        let off' = off + 1
                        writePositiveDec marr off' n qq                      -- digits
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        writePrimArray marr off minus                   -- leading minus
                        let off' = off + 1
                        setPrimArray marr off' leadingN zero            -- leading zeros
                        let off'' = off' + leadingN
                        writePositiveDec marr off'' n qq                 -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        setPrimArray marr off leadingN space            -- leading spaces
                        let off' = off + leadingN
                        writePrimArray marr off' minus                  -- leading minus
                        let off'' = off' + 1
                        writePositiveDec marr off'' n qq                 -- digits
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n'
                        writePrimArray marr off minus                   -- leading minus
                        let off' = off + 1
                        writePositiveDec marr off' n qq                  -- digits
                        let off'' = off' + n
                        setPrimArray marr off'' trailingN space         -- trailing spaces
            else
                writeN n' $ \marr off -> do
                    writePrimArray marr off minus                       -- leading minus
                    let off' = off + 1
                    writePositiveDec marr off' n qq                      -- digits
    | otherwise = positiveInt format i

positiveInt :: (Integral a) => IFormat -> a -> Builder ()
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Int    -> Builder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Int8   -> Builder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Int16  -> Builder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Int32  -> Builder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Int64  -> Builder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Word   -> Builder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Word8  -> Builder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Word16 -> Builder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Word32 -> Builder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Word64 -> Builder () #-}
positiveInt (IFormat width padding ps) i =
    let !n = countDigits i
    in if ps
        then
            let n' = n+1
            in if width > n'
            then case padding of
                NoPadding ->
                    writeN n' $ \marr off -> do
                        writePrimArray marr off plus                    -- leading plus
                        let off' = off + 1
                        writePositiveDec marr off' n i                   -- digits
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        writePrimArray marr off plus                    -- leading plus
                        let off' = off + 1
                        setPrimArray marr off' leadingN zero            -- leading zeros
                        let off'' = off' + leadingN
                        writePositiveDec marr off'' n i                  -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        setPrimArray marr off leadingN space            -- leading spaces
                        let off' = off + leadingN
                        writePrimArray marr off' plus                   -- leading plus
                        let off'' = off' + 1
                        writePositiveDec marr off'' n i                  -- digits
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n'
                        writePrimArray marr off plus                    -- leading plus
                        let off' = off + 1
                        writePositiveDec marr off' n i                   -- digits
                        let off'' = off' + n
                        setPrimArray marr off'' trailingN space         -- trailing spaces
            else
                writeN n' $ \marr off -> do
                    writePrimArray marr off plus                        -- leading plus
                    let off' = off + 1
                    writePositiveDec marr off' n i                       -- digits

        else if width > n
            then case padding of
                NoPadding ->
                    writeN n $ \marr off -> do
                        writePositiveDec marr off n i                    -- digits
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n
                        setPrimArray marr off leadingN zero             -- leading zeros
                        let off' = off + leadingN
                        writePositiveDec marr off' n i                   -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n
                        setPrimArray marr off leadingN space            -- leading spaces
                        let off' = off + leadingN
                        writePositiveDec marr off' n i                   -- digits
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n
                        writePositiveDec marr off n i                    -- digits
                        let off' = off + n
                        setPrimArray marr off' trailingN space          -- trailing spaces
            else
                writeN n $ \marr off -> do
                    writePositiveDec marr off n i                        -- digits

writePositiveDec :: (Integral a)
                => forall s. MutablePrimArray s Word8       -- ^ The buffer
                -> Int                                      -- ^ writing offset
                -> Int                                      -- ^ total digits
                -> a                                        -- ^ the value
                -> ST s ()
{-# INLINE writePositiveDec #-}
writePositiveDec marr off0 ds = go (off0 + ds - 1)
  where
    go off v
        | v >= 100 = do
            let (q, r) = v `quotRem` 100
            write2 off r
            go (off - 2) q
        | v < 10    = writePrimArray marr off (i2wDec v)
        | otherwise = write2 off v
    write2 off i0 = do
        let i = fromIntegral i0; j = i + i
        writePrimArray marr off $ indexOffAddr decDigitTable (j + 1)
        writePrimArray marr (off - 1) $ indexOffAddr decDigitTable j


--------------------------------------------------------------------------------
-- Below is an implementation of formatting integer, the main
-- idea is borrowed from base (GHC.Show).

#include "MachDeps.h"
#if SIZEOF_HSWORD == 4
#define DIGITS       9
#define BASE         1000000000
#elif SIZEOF_HSWORD == 8
#define DIGITS       18
#define BASE         1000000000000000000
#else
#error Please define DIGITS and BASE
-- DIGITS should be the largest integer such that
--     10^DIGITS < 2^(SIZEOF_HSWORD * 8 - 1)
-- BASE should be 10^DIGITS.
#endif

-- | Format a 'Integer' into decimal ASCII digits.
integer :: Integer -> Builder ()
#ifdef INTEGER_GMP
integer (S# i#) = int (I# i#)
#endif
-- Divide and conquer implementation of string conversion
integer n0
    | n0 < 0    = encodePrim minus >> integer' (-n0)
    | otherwise = integer' n0
  where
    integer' :: Integer -> Builder ()
    integer' n
        | n < BASE  = jhead (fromInteger n)
        | otherwise = jprinth (jsplitf (BASE*BASE) n)

    -- Convert a number that has been split into digits in base BASE^2
    -- this includes a last splitting step and then conversion of digits
    -- that all fit into a machine word.
    jprinth :: [Integer] -> Builder ()
    jprinth (n:ns) =
        case n `quotRemInteger` BASE of
        (# q', r' #) ->
            let q = fromInteger q'
                r = fromInteger r'
            in if q > 0 then jhead q >> jblock r >> jprintb ns
                        else jhead r >> jprintb ns
    jprinth [] = errorWithoutStackTrace "jprinth []"

    jprintb :: [Integer] -> Builder ()
    jprintb []     = return ()
    jprintb (n:ns) = case n `quotRemInteger` BASE of
                        (# q', r' #) ->
                            let q = fromInteger q'
                                r = fromInteger r'
                            in jblock q >> jblock r >> jprintb ns

    -- Convert an integer that fits into a machine word. Again, we have two
    -- functions, one that drops leading zeros (jhead) and one that doesn't
    -- (jblock)
    jhead :: Int -> Builder ()
    jhead = int
    jblock :: Int -> Builder ()
    jblock d = writeN DIGITS $ \ marr off -> writePositiveDec marr off DIGITS d

    -- Split n into digits in base p. We first split n into digits
    -- in base p*p and then split each of these digits into two.
    -- Note that the first 'digit' modulo p*p may have a leading zero
    -- in base p that we need to drop - this is what jsplith takes care of.
    -- jsplitb the handles the remaining digits.
    jsplitf :: Integer -> Integer -> [Integer]
    jsplitf p n
        | p > n     = [n]
        | otherwise = jsplith p (jsplitf (p*p) n)

    jsplith :: Integer -> [Integer] -> [Integer]
    jsplith p (n:ns) =
        case n `quotRemInteger` p of
        (# q, r #) ->
            if q > 0 then q : r : jsplitb p ns
                     else     r : jsplitb p ns
    jsplith _ [] = errorWithoutStackTrace "jsplith: []"

    jsplitb :: Integer -> [Integer] -> [Integer]
    jsplitb _ []     = []
    jsplitb p (n:ns) = case n `quotRemInteger` p of
                       (# q, r #) ->
                           q : r : jsplitb p ns

--------------------------------------------------------------------------------

-- | Count how many decimal digits an integer has.
countDigits :: (Integral a) => a -> Int
{-# INLINE countDigits #-}
countDigits v0
  | fromIntegral v64 == v0 = go 1 v64
  | otherwise              = goBig 1 (fromIntegral v0)
  where v64 = fromIntegral v0
        goBig !k (v :: Integer)
           | v > big   = goBig (k + 19) (v `quot` big)
           | otherwise = go k (fromIntegral v)
        big = 10000000000000000000
        go !k (v :: Word64)
           | v < 10    = k
           | v < 100   = k + 1
           | v < 1000  = k + 2
           | v < 1000000000000 =
               k + if v < 100000000
                   then if v < 1000000
                        then if v < 10000
                             then 3
                             else 4 + fin v 100000
                        else 6 + fin v 10000000
                   else if v < 10000000000
                        then 8 + fin v 1000000000
                        else 10 + fin v 100000000000
           | otherwise = go (k + 12) (v `quot` 1000000000000)
        fin v n = if v >= n then 1 else 0

minus, plus, zero, space :: Word8
{-# INLINE plus #-}
{-# INLINE minus #-}
{-# INLINE zero #-}
{-# INLINE space #-}
plus = 43
minus = 45
zero = 48
space = 32

-- | Decimal digit to ASCII digit.
i2wDec :: (Integral a) => a -> Word8
{-# INLINE i2wDec #-}
i2wDec v = zero + fromIntegral v

-- | Decimal digit to ASCII char.
i2cDec :: (Integral a) => a -> Char
{-# INLINE i2cDec #-}
i2cDec v = chr . fromIntegral $ zero + fromIntegral v

-- | Hexadecimal digit to ASCII char.
i2wHex :: (Integral a) => a -> Word8
{-# INLINE i2wHex #-}
i2wHex v
    | v <= 9    = zero + fromIntegral v
    | otherwise = 87 + fromIntegral v       -- fromEnum 'a' - 10

-- | Hexadecimal digit to UPPERCASED ASCII char.
i2wHeX :: (Integral a) => a -> Word8
{-# INLINE i2wHeX #-}
i2wHeX v
    | v <= 9    = zero + fromIntegral v
    | otherwise = 55 + fromIntegral v       -- fromEnum 'A' - 10

--------------------------------------------------------------------------------

-- | Format a 'FiniteBits' 'Integral' type into hex nibbles.
hex :: forall a. (FiniteBits a, Integral a) => a -> Builder ()
{-# SPECIALIZE INLINE hex :: Int    -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Int8   -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Int16  -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Int32  -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Int64  -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Word   -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Word8  -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Word16 -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Word32 -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Word64 -> Builder () #-}
hex w = writeN hexSize (go w (hexSize-2))
  where
    bitSize = finiteBitSize (undefined :: a)
    hexSize = (bitSize+3) `unsafeShiftR` 2
    go !v !d marr off
        | d > 0 = do
            let !i = fromIntegral v .&. 0xFF; !j = i + i
            writePrimArray marr (off + d) $ indexOffAddr hexDigitTable j
            writePrimArray marr (off + d + 1) $ indexOffAddr hexDigitTable (j+1)
            go (v `unsafeShiftR` 8) (d-2) marr off
        | d == 0 = do
            let !i = fromIntegral v .&. 0xFF; !j = i + i
            writePrimArray marr off $ indexOffAddr hexDigitTable j
            writePrimArray marr (off + 1) $ indexOffAddr hexDigitTable (j+1)
        | d < 0  = do         -- for FiniteBits instances which has extra bits
            let !i = fromIntegral v .&. 0x0F :: Int
            writePrimArray marr off $ i2wHex i


-- | The UPPERCASED version of 'hex'.
heX :: forall a. (FiniteBits a, Integral a) => a -> Builder ()
{-# SPECIALIZE INLINE heX :: Int    -> Builder () #-}
{-# SPECIALIZE INLINE heX :: Int8   -> Builder () #-}
{-# SPECIALIZE INLINE heX :: Int16  -> Builder () #-}
{-# SPECIALIZE INLINE heX :: Int32  -> Builder () #-}
{-# SPECIALIZE INLINE heX :: Int64  -> Builder () #-}
{-# SPECIALIZE INLINE heX :: Word   -> Builder () #-}
{-# SPECIALIZE INLINE heX :: Word8  -> Builder () #-}
{-# SPECIALIZE INLINE heX :: Word16 -> Builder () #-}
{-# SPECIALIZE INLINE heX :: Word32 -> Builder () #-}
{-# SPECIALIZE INLINE heX :: Word64 -> Builder () #-}
heX w = writeN hexSize (go w (hexSize-2))
  where
    bitSize = finiteBitSize (undefined :: a)
    hexSize = (bitSize+3) `unsafeShiftR` 2
    go !v !d marr off
        | d > 0 = do
            let !i = fromIntegral v .&. 0xFF; !j = i + i
            writePrimArray marr (off + d) $ indexOffAddr hexDigitTableUpper j
            writePrimArray marr (off + d + 1) $ indexOffAddr hexDigitTableUpper (j+1)
            go (v `unsafeShiftR` 8) (d-2) marr off
        | d == 0 = do
            let !i = fromIntegral v .&. 0xFF; !j = i + i
            writePrimArray marr off $ indexOffAddr hexDigitTableUpper j
            writePrimArray marr (off + 1) $ indexOffAddr hexDigitTableUpper (j+1)
        | d < 0  = do         -- for FiniteBits instances which has extra bits
            let !i = fromIntegral v .&. 0x0F :: Int
            writePrimArray marr off $ i2wHeX i

--------------------------------------------------------------------------------

-- Floating point numbers
-------------------------

-- | Control the rendering of floating point numbers.
data FFormat = Exponent -- ^ Scientific notation (e.g. @2.3e123@).
             | Fixed    -- ^ Standard decimal notation.
             | Generic  -- ^ Use decimal notation for values between @0.1@ and
                        -- @9,999,999@, and scientific notation otherwise.
           deriving (Enum, Read, Show)

-- | Decimal encoding of an IEEE 'Float'.
--
-- Using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
float :: Float -> Builder ()
{-# INLINE float #-}
float = floatWith Generic Nothing

-- | Decimal encoding of an IEEE 'Double'.
--
-- Using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
double :: Double -> Builder ()
{-# INLINE double #-}
double = doubleWith Generic Nothing

-- | Format single-precision float using drisu3 with dragon4 fallback.
floatWith :: FFormat
          -> Maybe Int  -- ^ Number of decimal places to render.
          -> Float
          -> Builder ()
{-# INLINE floatWith #-}
floatWith fmt decs x
    | isNaN x                   = "NaN"
    | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
    | x < 0                     = char8 '-' >> doFmt fmt decs (digits (-x))
    | isNegativeZero x          = char8 '-' >> doFmt fmt decs ([0], 0)
    | x == 0                    = doFmt fmt decs ([0], 0)
    | otherwise                 = doFmt fmt decs (digits x) -- Grisu only handles strictly positive finite numbers.
  where
    digits y = case grisu3_sp y of Just r  -> r
                                   Nothing -> floatToDigits 10 y

-- | Format double-precision float using drisu3 with dragon4 fallback.
doubleWith :: FFormat
           -> Maybe Int  -- ^ Number of decimal places to render.
           -> Double
           -> Builder ()
{-# INLINE doubleWith #-}
doubleWith fmt decs x
    | isNaN x                   = "NaN"
    | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
    | x < 0                     = char8 '-' >> doFmt fmt decs (digits (-x))
    | isNegativeZero x          = char8 '-' >> doFmt fmt decs ([0], 0)
    | x == 0                    = doFmt fmt decs ([0], 0)
    | otherwise                 = doFmt fmt decs (digits x) -- Grisu only handles strictly positive finite numbers.
  where
    digits y = case grisu3 y of Just r  -> r
                                Nothing -> floatToDigits 10 y

-- | Worker function to do formatting.
doFmt :: FFormat
      -> Maybe Int -- ^ Number of decimal places to render.
      -> ([Int], Int) -- ^ List of digits and exponent
      -> Builder ()
{-# INLINABLE doFmt #-}
doFmt format decs (is, e) =
    let ds = map i2cDec is
    in case format of
        Generic ->
            doFmt (if e < 0 || e > 7 then Exponent else Fixed) decs (is,e)
        Exponent ->
            case decs of
                Nothing ->
                    let show_e' = int (e-1)
                    in case ds of
                        "0"     -> "0.0e0"
                        [d]     -> char8 d >> ".0e" >> show_e'
                        (d:ds') -> char8 d >> char8 '.' >>
                                        string8 ds' >> char8 'e' >> show_e'
                        []      -> error "doFmt/Exponent: []"
                Just dec
                    | dec <= 0 ->
                    -- decimal point as well (ghc trac #15115).
                    -- Note that this handles negative precisions as well for consistency
                    -- (see ghc trac #15509).
                        case is of
                            [0] -> "0e0"
                            _ -> do
                                let (ei,is') = roundTo 10 1 is
                                    n:_ = map i2cDec (if ei > 0 then init is' else is')
                                char8 n
                                char8 'e'
                                int (e-1+ei)
                Just dec ->
                    let dec' = max dec 1 in
                    case is of
                        [0] -> do
                                char8 '0'
                                char8 '.'
                                replicateM dec' $ char8 '0'
                                char8 'e'
                                char8 '0'
                        _ -> do
                            let (ei,is') = roundTo 10 (dec'+1) is
                                (d:ds') = map i2cDec (if ei > 0 then init is' else is')
                            char8 d
                            char8 '.'
                            string8 ds'
                            char8 'e'
                            int (e-1+ei)
        Fixed ->
            let mk0 ls = case ls of { "" -> char8 '0' ; _ -> string8 ls}
            in case decs of
                Nothing
                    | e <= 0    -> do
                                char8 '0'
                                char8 '.'
                                replicateM (-e) $ char8 '0'
                                string8 ds
                    | otherwise ->
                        let f 0 s    rs  = mk0 (reverse s) >> char8 '.' >> mk0 rs
                            f n s    ""  = f (n-1) ('0':s) ""
                            f n s (r:rs) = f (n-1) (r:s) rs
                        in f e "" ds
                Just dec ->
                    let dec' = max dec 0
                    in if e >= 0
                        then
                            let (ei,is') = roundTo 10 (dec' + e) is
                                (ls,rs)  = splitAt (e+ei) (map i2cDec is')
                            in mk0 ls >>
                                (unless (List.null rs) $ char8 '.' >> string8 rs)
                        else
                            let (ei,is') = roundTo 10 dec' (List.replicate (-e) 0 ++ is)
                                d:ds' = map i2cDec (if ei > 0 then is' else 0:is')
                            in char8 d >>
                                (unless (List.null ds') $ char8 '.' >> string8 ds')

 ------------------------------------------------------------------------------
-- Conversion of 'Float's and 'Double's to ASCII in decimal using Grisu3
------------------------------------------------------------------------

#define GRISU3_SINGLE_BUF_LEN 10
#define GRISU3_DOUBLE_BUF_LEN 18

foreign import ccall unsafe "static grisu3" c_grisu3
    :: Double
    -> MutableByteArray# RealWorld  -- ^ char*
    -> MutableByteArray# RealWorld  -- ^ Int
    -> MutableByteArray# RealWorld  -- ^ Int
    -> IO Int

-- | Decimal encoding of a 'Double'.
grisu3 :: Double -> Maybe ([Int], Int)
{-# INLINE grisu3 #-}
grisu3 d = unsafePerformIO $
    withMutableByteArrayUnsafe GRISU3_DOUBLE_BUF_LEN $ \ pBuf -> do
        (len, (e, success)) <- withPrimUnsafe' $ \ pLen ->
            withPrimUnsafe' $ \ pE ->
                c_grisu3 (realToFrac d) pBuf pLen pE
        if success == 0 -- grisu3 fail
        then return Nothing
        else do
            buf <- forM [0..len-1] $ \ i -> do
                w8 <- readByteArray (MutableByteArray pBuf) i :: IO Word8
                return (fromIntegral w8)
            let !e' = e + len
            return $ Just (buf, e')

foreign import ccall unsafe "static grisu3_sp" c_grisu3_sp
    :: Float
    -> MutableByteArray# RealWorld  -- ^ char*
    -> MutableByteArray# RealWorld  -- ^ Int
    -> MutableByteArray# RealWorld  -- ^ Int
    -> IO Int

-- | Decimal encoding of a 'Float'.
grisu3_sp :: Float -> Maybe ([Int], Int)
{-# INLINE grisu3_sp #-}
grisu3_sp d = unsafePerformIO $
    withMutableByteArrayUnsafe GRISU3_SINGLE_BUF_LEN $ \ pBuf -> do
        (len, (e, success)) <- withPrimUnsafe' $ \ pLen ->
            withPrimUnsafe' $ \ pE ->
                c_grisu3_sp (realToFrac d) pBuf pLen pE
        if success == 0 -- grisu3 fail
        then return Nothing
        else do
            buf <- forM [0..len-1] $ \ i -> do
                w8 <- readByteArray (MutableByteArray pBuf) i :: IO Word8
                return (fromIntegral w8)
            let !e' = e + len
            return $ Just (buf, e')

--------------------------------------------------------------------------------

-- | A @Builder@ which renders a scientific number to full
-- precision, using standard decimal notation for arguments whose
-- absolute value lies between @0.1@ and @9,999,999@, and scientific
-- notation otherwise.
scientific :: Sci.Scientific -> Builder ()
{-# INLINE scientific #-}
scientific = scientificWith Generic Nothing

-- | Like 'scientific' but provides rendering options.
scientificWith :: FFormat
               -> Maybe Int  -- ^ Number of decimal places to render.
               -> Sci.Scientific
               -> Builder ()
{-# INLINE scientificWith #-}
scientificWith fmt decs scntfc
   | scntfc < 0 = char8 '-' <> doFmt fmt decs (Sci.toDecimalDigits (-scntfc))
   | otherwise  =              doFmt fmt decs (Sci.toDecimalDigits   scntfc)
