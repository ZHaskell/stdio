{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Std.Data.Builder.Textual
  (
  -- * Textual Builder
    TextualBuilder(..)
  , buildText
  -- * Integral type formatting
  , IFormat(..)
  , Base(..)
  , defaultIFormat
  , Padding(..)
  , int
  , intWith
  , integer
  , integerWith
  -- * Fixded size hexidecimal formatting
  , hex, heX
  -- * IEEE float formating
  , FFFormat(..)
  , float
  , double
  , floatWith
  , doubleWith

  {-
  , char7
  , string7

  , char8
  , string8

  , charUtf8
  , stringUtf8
  , text
  , show
  -}
  ) where

import Std.Data.Builder.Base
import Std.Data.Builder.DigitTable
import Std.Data.Text.Base
import Data.Primitive.PrimArray
import Data.Primitive.Addr
import Data.Int
import Data.Word
import Data.Bits
import Control.Monad.ST
import GHC.Integer
import GHC.Types
#ifdef INTEGER_GMP
import GHC.Integer.GMP.Internals
#endif
import GHC.Float (FFFormat(..))

-- | Buidlers which are safely UTF-8 encoded, thus can be used to build
-- text directly.
--
-- Use 'toBuilder' to convert a 'TextualBuilder' to bytes 'Builder'.
newtype TextualBuilder a = TextualBuilder { toBuilder :: Builder a }
    deriving (Functor, Applicative, Monad)

buildText :: TextualBuilder a -> Text
{-# INLINE buildText #-}
buildText = Text . buildBytes . toBuilder

--------------------------------------------------------------------------------

-- | Integral formatting options.
--
data IFormat = IFormat
    { width :: Int              -- ^ total width, only effective with padding options
    , padding :: Padding        -- ^ padding options
    , postiveSign :: Bool       -- ^ show @+@ when the number is positive
    , base :: Base              -- ^ choose between bases
    } deriving (Show, Eq, Ord)

-- | @defaultIFormat = IFormat 0 NoPadding False Decimal@
defaultIFormat :: IFormat
defaultIFormat = IFormat 0 NoPadding False Decimal

data Padding = NoPadding | ZeroPadding | LeftSpacePadding | RightSpacePadding deriving (Show, Eq, Ord)
data Base = Decimal | HexadecimalLower | HexadecimalUpper deriving (Show, Eq, Ord)

-- | @int = intWith defaultIFormat@
int :: (Integral a, Bounded a) => a -> TextualBuilder ()
int = intWith defaultIFormat

-- | Format a 'Bounded' 'Integral' type like @Int@ or @Word16@ into ascii digits.
--
intWith :: (Integral a, Bounded a)
        => IFormat
        -> a
        -> TextualBuilder ()
{-# INLINE[1] intWith #-}
{-# RULES "intWith'/Int8"    intWith = intWith' :: IFormat -> Int8    -> TextualBuilder () #-}
{-# RULES "intWith'/Int"     intWith = intWith' :: IFormat -> Int     -> TextualBuilder () #-}
{-# RULES "intWith'/Int16"   intWith = intWith' :: IFormat -> Int16   -> TextualBuilder () #-}
{-# RULES "intWith'/Int32"   intWith = intWith' :: IFormat -> Int32   -> TextualBuilder () #-}
{-# RULES "intWith'/Int64"   intWith = intWith' :: IFormat -> Int64   -> TextualBuilder () #-}
{-# RULES "intWith'/Word"    intWith = positiveInt  :: IFormat -> Word    -> TextualBuilder () #-}
{-# RULES "intWith'/Word8"   intWith = positiveInt  :: IFormat -> Word8   -> TextualBuilder () #-}
{-# RULES "intWith'/Word16"  intWith = positiveInt  :: IFormat -> Word16  -> TextualBuilder () #-}
{-# RULES "intWith'/Word32"  intWith = positiveInt  :: IFormat -> Word32  -> TextualBuilder () #-}
{-# RULES "intWith'/Word64"  intWith = positiveInt  :: IFormat -> Word64  -> TextualBuilder () #-}
intWith = intWith'

intWith' :: (Integral a, Bounded a) => IFormat -> a -> TextualBuilder ()
{-# SPECIALIZE INLINE intWith' :: IFormat -> Int   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE intWith' :: IFormat -> Int8  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE intWith' :: IFormat -> Int16 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE intWith' :: IFormat -> Int32 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE intWith' :: IFormat -> Int64 -> TextualBuilder () #-}
intWith' format@(IFormat width padding _ Decimal) i
    | i < 0 = TextualBuilder $
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
intWith' format@(IFormat width padding _ b) i
    | i < 0 = TextualBuilder $
        if i == minBound            -- can't directly negate in this case
        then do
            let (q, r) = i `quotRem` 0x10
                !qq = -q            -- all digits except last one
                !rr = i2wHex b (-r)  -- last digits
                !n = countHexDigits qq
                !n' = n + 2         -- extra two bytes: minus and last digit
            if width > n'
            then case padding of
                NoPadding ->
                    writeN n' $ \marr off -> do
                        writePrimArray marr off minus                       -- leading minus
                        let off' = off + 1
                        writePositiveHex b marr off' n qq                      -- digits
                        let off'' = off' + n
                        writePrimArray marr off'' rr                        -- last digit
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        writePrimArray marr off minus                   -- leading minus
                        let off' = off + 1
                        setPrimArray marr off' leadingN zero            -- leading zeros
                        let off'' = off' + leadingN
                        writePositiveHex b marr off'' n qq                 -- digits
                        let off''' = off'' + n
                        writePrimArray marr off''' rr                   -- last digit
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        setPrimArray marr off leadingN space            -- leading spaces
                        let off' = off + leadingN
                        writePrimArray marr off' minus                  -- leading minus
                        let off'' = off' + 1
                        writePositiveHex b marr off'' n qq                 -- digits
                        let off''' = off'' + n
                        writePrimArray marr off''' rr                   -- last digit
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n'
                        writePrimArray marr off minus                   -- leading minus
                        let off' = off + 1
                        writePositiveHex b marr off' n qq                  -- digits
                        let off'' = off' + n
                        writePrimArray marr off'' rr                    -- last digit
                        let off''' = off'' + 1
                        setPrimArray marr off''' trailingN space        -- trailing spaces
            else
                writeN n' $ \marr off -> do
                    writePrimArray marr off minus                       -- leading minus
                    let off' = off + 1
                    writePositiveHex b marr off' n qq                      -- digits
                    let off'' = off' + n
                    writePrimArray marr off'' rr                        -- last digit
        else do
            let !qq = -i
                !n = countHexDigits qq
                !n' = n + 1  -- extra byte: minus
            if width > n'
            then case padding of
                NoPadding ->
                    writeN n' $ \marr off -> do
                        writePrimArray marr off minus                       -- leading minus
                        let off' = off + 1
                        writePositiveHex b marr off' n qq                      -- digits
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        writePrimArray marr off minus                   -- leading minus
                        let off' = off + 1
                        setPrimArray marr off' leadingN zero            -- leading zeros
                        let off'' = off' + leadingN
                        writePositiveHex b marr off'' n qq                 -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        setPrimArray marr off leadingN space            -- leading spaces
                        let off' = off + leadingN
                        writePrimArray marr off' minus                  -- leading minus
                        let off'' = off' + 1
                        writePositiveHex b marr off'' n qq                 -- digits
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n'
                        writePrimArray marr off minus                   -- leading minus
                        let off' = off + 1
                        writePositiveHex b marr off' n qq                  -- digits
                        let off'' = off' + n
                        setPrimArray marr off'' trailingN space         -- trailing spaces
            else
                writeN n' $ \marr off -> do
                    writePrimArray marr off minus                       -- leading minus
                    let off' = off + 1
                    writePositiveHex b marr off' n qq                      -- digits
    | otherwise = positiveInt format i

positiveInt :: (Integral a) => IFormat -> a -> TextualBuilder ()
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Int    -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Int8   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Int16  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Int32  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Int64  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Word   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Word8  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Word16 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Word32 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveInt :: IFormat -> Word64 -> TextualBuilder () #-}
positiveInt (IFormat width padding ps Decimal) i =
    let !n = countDigits i
    in TextualBuilder $
        if ps
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

        else
            if width > n
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
positiveInt (IFormat width padding ps b) i =
    let !n = countHexDigits i
    in TextualBuilder $
        if ps
        then
            let n' = n+1
            in if width > n'
            then case padding of
                NoPadding ->
                    writeN n' $ \marr off -> do
                        writePrimArray marr off plus                    -- leading plus
                        let off' = off + 1
                        writePositiveHex b marr off' n i                   -- digits
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        writePrimArray marr off plus                    -- leading plus
                        let off' = off + 1
                        setPrimArray marr off' leadingN zero            -- leading zeros
                        let off'' = off' + leadingN
                        writePositiveHex b marr off'' n i                  -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        setPrimArray marr off leadingN space            -- leading spaces
                        let off' = off + leadingN
                        writePrimArray marr off' plus                   -- leading plus
                        let off'' = off' + 1
                        writePositiveHex b marr off'' n i                  -- digits
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n'
                        writePrimArray marr off plus                    -- leading plus
                        let off' = off + 1
                        writePositiveHex b marr off' n i                   -- digits
                        let off'' = off' + n
                        setPrimArray marr off'' trailingN space         -- trailing spaces
            else
                writeN n' $ \marr off -> do
                    writePrimArray marr off plus                        -- leading plus
                    let off' = off + 1
                    writePositiveHex b marr off' n i                       -- digits

        else
            if width > n
            then case padding of
                NoPadding ->
                    writeN n $ \marr off -> do
                        writePositiveHex b marr off n i                    -- digits
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n
                        setPrimArray marr off leadingN zero             -- leading zeros
                        let off' = off + leadingN
                        writePositiveHex b marr off' n i                   -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n
                        setPrimArray marr off leadingN space            -- leading spaces
                        let off' = off + leadingN
                        writePositiveHex b marr off' n i                   -- digits
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n
                        writePositiveHex b marr off n i                    -- digits
                        let off' = off + n
                        setPrimArray marr off' trailingN space          -- trailing spaces
            else
                writeN n $ \marr off -> do
                    writePositiveHex b marr off n i                        -- digits

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

writePositiveHex :: (Integral a)
                => forall s. Base -> MutablePrimArray s Word8 -> Int -> Int -> a -> ST s ()
{-# INLINE writePositiveHex #-}
writePositiveHex b marr off0 ds = go (off0 + ds - 1)
  where
    go off v
        | v >= 0x100 = do
            let (q, r) = v `quotRem` 0x100
            write2 off r
            go (off - 2) q
        | v < 0x10    = writePrimArray marr off (i2wHex b v)
        | otherwise = write2 off v
    write2 off i0 = do
        let i = fromIntegral i0; j = i + i
            table = case b of HexadecimalLower -> hexDigitTable
                              _                -> hexDigitTableUpper
        writePrimArray marr off $ indexOffAddr table (j + 1)
        writePrimArray marr (off - 1) $ indexOffAddr table j

--------------------------------------------------------------------------------
-- Below is an implementation of formatting integer, the main
-- idea is borrowed from base (GHC.Show).

#include "MachDeps.h"
#if SIZEOF_HSWORD == 4
#define DIGITS       9
#define BASE         1000000000
#define HEX_DIGITS   7
#define HEX_BASE     268435456
#elif SIZEOF_HSWORD == 8
#define DIGITS       18
#define BASE         1000000000000000000
#define HEX_DIGITS   15
#define HEX_BASE     1152921504606846976
#else
#error Please define DIGITS and BASE
-- DIGITS should be the largest integer such that
--     10^DIGITS < 2^(SIZEOF_HSWORD * 8 - 1)
-- BASE should be 10^DIGITS.
-- HEX_DIGITS should be the largest integer such that
--     16^HEX_DIGITS < 2^(SIZEOF_HSWORD * 8 - 1)
-- HEX_BASE should be 16^DIGITS.
#endif

integer :: Integer -> TextualBuilder ()
integer = integerWith Decimal

integerWith :: Base -> Integer -> TextualBuilder ()
#ifdef INTEGER_GMP
integerWith b (S# i#) = intWith defaultIFormat{base = b} (I# i#)
#endif
-- Divide and conquer implementation of string conversion
integerWith Decimal n0
    | n0 < 0    = TextualBuilder $ encodePrim minus >> integerWith' (-n0)
    | otherwise = TextualBuilder $ integerWith' n0
  where
    integerWith' :: Integer -> Builder ()
    integerWith' n
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
    jhead = toBuilder . int
    jblock :: Int -> Builder ()
    jblock d = writeN DIGITS $ \ marr off -> writePositiveDec marr off DIGITS d

integerWith b n0
    | n0 < 0    = TextualBuilder $ encodePrim minus >> integerWith' b (-n0)
    | otherwise = TextualBuilder $ integerWith' b n0
  where
    integerWith' :: Base -> Integer -> Builder ()
    integerWith' b = \ n ->
        if n < HEX_BASE
        then jhead (fromInteger n)
        else jprinth (jsplitf (HEX_BASE*HEX_BASE) n)

    jprinth :: [Integer] -> Builder ()
    jprinth (n:ns) =
        case n `quotRemInteger` HEX_BASE of
        (# q', r' #) ->
            let q = fromInteger q'
                r = fromInteger r'
            in if q > 0 then jhead q >> jblock r >> jprintb ns
                        else jhead r >> jprintb ns
    jprinth [] = errorWithoutStackTrace "jprinth []"

    jprintb :: [Integer] -> Builder ()
    jprintb []     = return ()
    jprintb (n:ns) = case n `quotRemInteger` HEX_BASE of
                        (# q', r' #) ->
                            let q = fromInteger q'
                                r = fromInteger r'
                            in jblock q >> jblock r >> jprintb ns
    jhead :: Int -> Builder ()
    jhead = toBuilder . intWith defaultIFormat{ base = b }
    jblock :: Int -> Builder ()
    jblock d = writeN HEX_DIGITS $ \ marr off -> writePositiveHex b marr off HEX_DIGITS d

--------------------------------------------------------------------------------

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

countHexDigits :: (Integral a) => a -> Int
{-# INLINE countHexDigits #-}
countHexDigits v0
  | fromIntegral v64 == v0 = go 1 v64
  | otherwise              = goBig 1 (fromIntegral v0)
  where v64 = fromIntegral v0
        goBig !k (v :: Integer)
           | v > big   = goBig (k + 18) (v `quot` big)
           | otherwise = go k (fromIntegral v)
        big = 0x1000000000000000000
        go !k (v :: Word64)
           | v < 0x10    = k
           | v < 0x100   = k + 1
           | v < 0x1000  = k + 2
           | v < 0x1000000000000 =
               k + if v < 0x100000000
                   then if v < 0x1000000
                        then if v < 0x10000
                             then 3
                             else 4 + fin v 0x100000
                        else 6 + fin v 0x10000000
                   else if v < 0x10000000000
                        then 8 + fin v 0x1000000000
                        else 10 + fin v 0x100000000000
           | otherwise = go (k + 12) (v `quot` 0x1000000000000)
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

i2wDec :: (Integral a) => a -> Word8
{-# INLINE i2wDec #-}
i2wDec v = zero + fromIntegral v

i2wHex :: (Integral a) => Base -> a -> Word8
{-# INLINE i2wHex #-}
i2wHex HexadecimalLower v
    | v <= 9    = zero + fromIntegral v
    | otherwise = 87 + fromIntegral v       -- fromEnum 'a' - 10
i2wHex HexadecimalUpper v
    | v <= 9    = zero + fromIntegral v
    | otherwise = 55 + fromIntegral v       -- fromEnum 'A' - 10

--------------------------------------------------------------------------------

-- | Format a 'FiniteBits' 'Integral' type into hex nibbles.
--
hex :: forall a. (FiniteBits a, Integral a) => a -> TextualBuilder ()
{-# SPECIALIZE INLINE hex :: Int    -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hex :: Int8   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hex :: Int16  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hex :: Int32  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hex :: Int64  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hex :: Word   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hex :: Word8  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hex :: Word16 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hex :: Word32 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hex :: Word64 -> TextualBuilder () #-}
hex w = TextualBuilder $ writeN hexSize (go (bitSize-8))
  where
    bitSize = finiteBitSize (undefined :: a)
    hexSize = bitSize `unsafeShiftR` 2
    go !b marr off
        | b > 0 = do
            let !i = fromIntegral (w `unsafeShiftR` b) .&. 0xFF; !j = i + i
            writePrimArray marr off $ indexOffAddr hexDigitTable j
            writePrimArray marr (off + 1) $ indexOffAddr hexDigitTable (j+1)
            go (b-8) marr (off+2)
        | otherwise = do
            let !i = fromIntegral w .&. 0xFF; !j = i + i
            writePrimArray marr off $ indexOffAddr hexDigitTable j
            writePrimArray marr (off + 1) $ indexOffAddr hexDigitTable (j+1)

-- | The uppercase version of 'hex'.
--
heX :: forall a. (FiniteBits a, Integral a) => a -> TextualBuilder ()
{-# SPECIALIZE INLINE heX :: Int    -> TextualBuilder () #-}
{-# SPECIALIZE INLINE heX :: Int8   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE heX :: Int16  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE heX :: Int32  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE heX :: Int64  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE heX :: Word   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE heX :: Word8  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE heX :: Word16 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE heX :: Word32 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE heX :: Word64 -> TextualBuilder () #-}
heX w = TextualBuilder $ writeN hexSize (go (bitSize-8))
  where
    bitSize = finiteBitSize (undefined :: a)
    hexSize = bitSize `unsafeShiftR` 2
    go !b marr off
        | b > 0 = do
            let !i = fromIntegral (w `unsafeShiftR` b) .&. 0xFF; !j = i + i
            writePrimArray marr off $ indexOffAddr hexDigitTableUpper j
            writePrimArray marr (off + 1) $ indexOffAddr hexDigitTableUpper (j+1)
            go (b-8) marr (off+2)
        | otherwise = do
            let !i = fromIntegral w .&. 0xFF; !j = i + i
            writePrimArray marr off $ indexOffAddr hexDigitTableUpper j
            writePrimArray marr (off + 1) $ indexOffAddr hexDigitTableUpper (j+1)

--------------------------------------------------------------------------------

float :: Float -> TextualBuilder ()
float = floatWith FFGeneric
double :: Double -> TextualBuilder ()
double = doubleWith FFGeneric

floatWith :: FFFormat -> Float -> TextualBuilder ()
floatWith = undefined
doubleWith :: FFFormat -> Double -> TextualBuilder ()
doubleWith = undefined

char7 :: Char -> TextualBuilder ()
char7 = undefined
string7 :: String -> TextualBuilder ()
string7 = undefined

char8 :: Char -> TextualBuilder ()
char8 = undefined
string8 :: String -> TextualBuilder ()
string8 = undefined

char :: Char -> TextualBuilder ()
char = undefined
string :: String -> TextualBuilder ()
string = undefined

text :: Text -> TextualBuilder ()
text = undefined
show :: Show a => a -> TextualBuilder ()
show = undefined

