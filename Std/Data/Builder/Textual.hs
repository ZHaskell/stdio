{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Std.Data.Builder.Textual
  (
    TextualBuilder(..)
  , buildText
  , IFormat(..)
  , defaultIFormat
  , Padding(..)
  , dec
  , decWith
  , hex
  , hexWith
  {-
  , FFFormat(..)
  , float
  , double
  , floatWith
  , doubleWith

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
import Control.Monad.ST
import GHC.Float (FFFormat(..))

-- | Buidlers which are safely UTF-8 encoded, thus can be used to build
-- text directly.
--
-- Use 'textualBuilder' to use a 'TextualBuilder' as bytes 'Builder'.
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
    , upperCase :: Bool         -- ^ Hex digits' casing, only effective with hex formatting.
    } deriving (Show, Eq, Ord)

defaultIFormat :: IFormat
defaultIFormat = IFormat 0 NoPadding False False

data Padding = NoPadding | ZeroPadding | LeftSpacePadding | RightSpacePadding deriving (Show, Eq, Ord)

-- | @dec = decWith defaultIFormat@
dec :: (Integral a, Bounded a) => a -> TextualBuilder ()
dec = decWith defaultIFormat

decWith :: (Integral a, Bounded a)
        => IFormat
        -> a
        -> TextualBuilder ()
{-# INLINE[1] decWith #-}
{-# RULES "decWith'/Int8"    decWith = decWith' :: IFormat -> Int8    -> TextualBuilder () #-}
{-# RULES "decWith'/Int"     decWith = decWith' :: IFormat -> Int     -> TextualBuilder () #-}
{-# RULES "decWith'/Int16"   decWith = decWith' :: IFormat -> Int16   -> TextualBuilder () #-}
{-# RULES "decWith'/Int32"   decWith = decWith' :: IFormat -> Int32   -> TextualBuilder () #-}
{-# RULES "decWith'/Int64"   decWith = decWith' :: IFormat -> Int64   -> TextualBuilder () #-}
{-# RULES "decWith'/Word"    decWith = positiveDec  :: IFormat -> Word    -> TextualBuilder () #-}
{-# RULES "decWith'/Word8"   decWith = positiveDec  :: IFormat -> Word8   -> TextualBuilder () #-}
{-# RULES "decWith'/Word16"  decWith = positiveDec  :: IFormat -> Word16  -> TextualBuilder () #-}
{-# RULES "decWith'/Word32"  decWith = positiveDec  :: IFormat -> Word32  -> TextualBuilder () #-}
{-# RULES "decWith'/Word64"  decWith = positiveDec  :: IFormat -> Word64  -> TextualBuilder () #-}
decWith = decWith'

decWith' :: (Integral a, Bounded a) => IFormat -> a -> TextualBuilder ()
{-# SPECIALIZE INLINE decWith' :: IFormat -> Int   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE decWith' :: IFormat -> Int8  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE decWith' :: IFormat -> Int16 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE decWith' :: IFormat -> Int32 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE decWith' :: IFormat -> Int64 -> TextualBuilder () #-}
decWith' format@(IFormat width padding _ _) i
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
    | otherwise = positiveDec format i

positiveDec :: (Integral a) => IFormat -> a -> TextualBuilder ()
{-# SPECIALIZE INLINE positiveDec :: IFormat -> Int    -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveDec :: IFormat -> Int8   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveDec :: IFormat -> Int16  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveDec :: IFormat -> Int32  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveDec :: IFormat -> Int64  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveDec :: IFormat -> Word   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveDec :: IFormat -> Word8  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveDec :: IFormat -> Word16 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveDec :: IFormat -> Word32 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveDec :: IFormat -> Word64 -> TextualBuilder () #-}
positiveDec (IFormat width padding ps _) i =
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


writePositiveDec :: (Integral a)
                => forall s. MutablePrimArray s Word8 -> Int -> Int -> a -> ST s ()
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

-- | @dec = decWith defaultIFormat@
hex :: (Integral a, Bounded a) => a -> TextualBuilder ()
hex = hexWith defaultIFormat

hexWith :: (Integral a, Bounded a)
        => IFormat
        -> a
        -> TextualBuilder ()
{-# INLINE[1] hexWith #-}
{-# RULES "hexWith'/Int8"    hexWith = hexWith' :: IFormat -> Int8    -> TextualBuilder () #-}
{-# RULES "hexWith'/Int"     hexWith = hexWith' :: IFormat -> Int     -> TextualBuilder () #-}
{-# RULES "hexWith'/Int16"   hexWith = hexWith' :: IFormat -> Int16   -> TextualBuilder () #-}
{-# RULES "hexWith'/Int32"   hexWith = hexWith' :: IFormat -> Int32   -> TextualBuilder () #-}
{-# RULES "hexWith'/Int64"   hexWith = hexWith' :: IFormat -> Int64   -> TextualBuilder () #-}
{-# RULES "hexWith'/Word"    hexWith = positiveHex  :: IFormat -> Word    -> TextualBuilder () #-}
{-# RULES "hexWith'/Word8"   hexWith = positiveHex  :: IFormat -> Word8   -> TextualBuilder () #-}
{-# RULES "hexWith'/Word16"  hexWith = positiveHex  :: IFormat -> Word16  -> TextualBuilder () #-}
{-# RULES "hexWith'/Word32"  hexWith = positiveHex  :: IFormat -> Word32  -> TextualBuilder () #-}
{-# RULES "hexWith'/Word64"  hexWith = positiveHex  :: IFormat -> Word64  -> TextualBuilder () #-}
hexWith = hexWith'

hexWith' :: (Integral a, Bounded a) => IFormat -> a -> TextualBuilder ()
{-# SPECIALIZE INLINE hexWith' :: IFormat -> Int   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hexWith' :: IFormat -> Int8  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hexWith' :: IFormat -> Int16 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hexWith' :: IFormat -> Int32 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE hexWith' :: IFormat -> Int64 -> TextualBuilder () #-}
hexWith' format@(IFormat width padding _ upper) i
    | i < 0 = TextualBuilder $
        if i == minBound            -- can't directly negate in this case
        then do
            let (q, r) = i `quotRem` 0x10
                !qq = -q            -- all digits except last one
                !rr = i2wHex upper (-r)  -- last digits
                !n = countHexDigits qq
                !n' = n + 2         -- extra two bytes: minus and last digit
            if width > n'
            then case padding of
                NoPadding ->
                    writeN n' $ \marr off -> do
                        writePrimArray marr off minus                       -- leading minus
                        let off' = off + 1
                        writePositiveHex upper marr off' n qq                      -- digits
                        let off'' = off' + n
                        writePrimArray marr off'' rr                        -- last digit
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        writePrimArray marr off minus                   -- leading minus
                        let off' = off + 1
                        setPrimArray marr off' leadingN zero            -- leading zeros
                        let off'' = off' + leadingN
                        writePositiveHex upper marr off'' n qq                 -- digits
                        let off''' = off'' + n
                        writePrimArray marr off''' rr                   -- last digit
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        setPrimArray marr off leadingN space            -- leading spaces
                        let off' = off + leadingN
                        writePrimArray marr off' minus                  -- leading minus
                        let off'' = off' + 1
                        writePositiveHex upper marr off'' n qq                 -- digits
                        let off''' = off'' + n
                        writePrimArray marr off''' rr                   -- last digit
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n'
                        writePrimArray marr off minus                   -- leading minus
                        let off' = off + 1
                        writePositiveHex upper marr off' n qq                  -- digits
                        let off'' = off' + n
                        writePrimArray marr off'' rr                    -- last digit
                        let off''' = off'' + 1
                        setPrimArray marr off''' trailingN space        -- trailing spaces
            else
                writeN n' $ \marr off -> do
                    writePrimArray marr off minus                       -- leading minus
                    let off' = off + 1
                    writePositiveHex upper marr off' n qq                      -- digits
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
                        writePositiveHex upper marr off' n qq                      -- digits
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        writePrimArray marr off minus                   -- leading minus
                        let off' = off + 1
                        setPrimArray marr off' leadingN zero            -- leading zeros
                        let off'' = off' + leadingN
                        writePositiveHex upper marr off'' n qq                 -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        setPrimArray marr off leadingN space            -- leading spaces
                        let off' = off + leadingN
                        writePrimArray marr off' minus                  -- leading minus
                        let off'' = off' + 1
                        writePositiveHex upper marr off'' n qq                 -- digits
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n'
                        writePrimArray marr off minus                   -- leading minus
                        let off' = off + 1
                        writePositiveHex upper marr off' n qq                  -- digits
                        let off'' = off' + n
                        setPrimArray marr off'' trailingN space         -- trailing spaces
            else
                writeN n' $ \marr off -> do
                    writePrimArray marr off minus                       -- leading minus
                    let off' = off + 1
                    writePositiveHex upper marr off' n qq                      -- digits
    | otherwise = positiveHex format i

positiveHex :: (Integral a) => IFormat -> a -> TextualBuilder ()
{-# SPECIALIZE INLINE positiveHex :: IFormat -> Int    -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveHex :: IFormat -> Int8   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveHex :: IFormat -> Int16  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveHex :: IFormat -> Int32  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveHex :: IFormat -> Int64  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveHex :: IFormat -> Word   -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveHex :: IFormat -> Word8  -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveHex :: IFormat -> Word16 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveHex :: IFormat -> Word32 -> TextualBuilder () #-}
{-# SPECIALIZE INLINE positiveHex :: IFormat -> Word64 -> TextualBuilder () #-}
positiveHex (IFormat width padding ps upper) i =
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
                        writePositiveHex upper marr off' n i                   -- digits
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        writePrimArray marr off plus                    -- leading plus
                        let off' = off + 1
                        setPrimArray marr off' leadingN zero            -- leading zeros
                        let off'' = off' + leadingN
                        writePositiveHex upper marr off'' n i                  -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        setPrimArray marr off leadingN space            -- leading spaces
                        let off' = off + leadingN
                        writePrimArray marr off' plus                   -- leading plus
                        let off'' = off' + 1
                        writePositiveHex upper marr off'' n i                  -- digits
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n'
                        writePrimArray marr off plus                    -- leading plus
                        let off' = off + 1
                        writePositiveHex upper marr off' n i                   -- digits
                        let off'' = off' + n
                        setPrimArray marr off'' trailingN space         -- trailing spaces
            else
                writeN n' $ \marr off -> do
                    writePrimArray marr off plus                        -- leading plus
                    let off' = off + 1
                    writePositiveHex upper marr off' n i                       -- digits

        else
            if width > n
            then case padding of
                NoPadding ->
                    writeN n $ \marr off -> do
                        writePositiveHex upper marr off n i                    -- digits
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n
                        setPrimArray marr off leadingN zero             -- leading zeros
                        let off' = off + leadingN
                        writePositiveHex upper marr off' n i                   -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n
                        setPrimArray marr off leadingN space            -- leading spaces
                        let off' = off + leadingN
                        writePositiveHex upper marr off' n i                   -- digits
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n
                        writePositiveHex upper marr off n i                    -- digits
                        let off' = off + n
                        setPrimArray marr off' trailingN space          -- trailing spaces
            else
                writeN n $ \marr off -> do
                    writePositiveHex upper marr off n i                        -- digits


writePositiveHex :: (Integral a)
                => forall s. Bool -> MutablePrimArray s Word8 -> Int -> Int -> a -> ST s ()
{-# INLINE writePositiveHex #-}
writePositiveHex upper marr off0 ds = go (off0 + ds - 1)
  where
    go off v
        | v >= 0x100 = do
            let (q, r) = v `quotRem` 0x100
            write2 off r
            go (off - 2) q
        | v < 0x10    = writePrimArray marr off (i2wHex upper v)
        | otherwise = write2 off v
    write2 off i0 = do
        let i = fromIntegral i0; j = i + i
            table = if upper then hexDigitTableUpper else hexDigitTable
        writePrimArray marr off $ indexOffAddr table (j + 1)
        writePrimArray marr (off - 1) $ indexOffAddr table j

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
{-
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
#ifdef INTEGER_GMP
integer 10 f (S# i#) = hex (I# i#)
integer 16 f (S# i#) = hex (I# i#)
#endif
integer base i
    | i < 0     = TextualBuilder $ encodePrim minus >> go (-i)
    | otherwise = TextualBuilder $ go i
  where
    go n | n < maxInt = int (fromInteger n)
         | otherwise  = putH (splitf (maxInt * maxInt) n)

    splitf p n
      | p > n       = [n]
      | otherwise   = splith p (splitf (p*p) n)

    splith p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) | q > 0     -> q : r : splitb p ns
                                  | otherwise -> r : splitb p ns
    splith _ _      = error "splith: the impossible happened."

    splitb p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) -> q : r : splitb p ns
    splitb _ _      = []

    T maxInt10 maxDigits10 =
        until ((>mi) . (*10) . fstT) (\(T n d) -> T (n*10) (d+1)) (T 10 1)
      where mi = fromIntegral (maxBound :: Int)
    T maxInt16 maxDigits16 =
        until ((>mi) . (*16) . fstT) (\(T n d) -> T (n*16) (d+1)) (T 16 1)
      where mi = fromIntegral (maxBound :: Int)

    fstT (T a _) = a

    maxInt | base == 10 = maxInt10
           | otherwise  = maxInt16
    maxDigits | base == 10 = maxDigits10
              | otherwise  = maxDigits16

    putH (n:ns) = case n `quotRemInteger` maxInt of
                    PAIR(x,y)
                        | q > 0     -> int q <> pblock r <> putB ns
                        | otherwise -> int r <> putB ns
                        where q = fromInteger x
                              r = fromInteger y
    putH _ = error "putH: the impossible happened"

    putB (n:ns) = case n `quotRemInteger` maxInt of
                    PAIR(x,y) -> pblock q <> pblock r <> putB ns
                        where q = fromInteger x
                              r = fromInteger y
    putB _ = return ()

    int :: Int -> Builder ()
    int x | base == 10 = dec x
          | otherwise  = hex x

    pblock = loop maxDigits
      where
        loop !d !n
            | d == 1    = hexDigit n
            | otherwise = loop (d-1) q <> hexDigit r
            where q = n `quotInt` base
                  r = n `remInt` base
-}

--------------------------------------------------------------------------------

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

i2wHex :: (Integral a) => Bool -> a -> Word8
{-# INLINE i2wHex #-}
i2wHex upper v
    | v <= 9    = zero + fromIntegral v
    | upper     = 55 + fromIntegral v       -- fromEnum 'A' - 10
    | otherwise = 87 + fromIntegral v       -- fromEnum 'a' - 10

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

