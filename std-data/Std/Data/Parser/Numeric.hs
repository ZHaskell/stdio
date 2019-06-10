{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE CPP               #-}

{-|
Module      : Std.Data.Parser.Numeric
Description : Textual numeric parsers.
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Textual numeric parsers.

-}

module Std.Data.Parser.Numeric
  ( -- * Decimal
    uint, int
    -- * Hex
  , hex
    -- * Fractional
  , rational
  , float, double
  , scientific
  , scientifically
    -- * Stricter fractional(rfc8259)
  , rational'
  , float', double'
  , scientific'
  , scientifically'
    -- * Misc
  , hexLoop
  , decLoop
  , decLoopIntegerFast
  , isHexDigit
  , isDigit
  , floatToScientific
  , doubleToScientific
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Int
import qualified Data.Scientific          as Sci
import           Data.Word
import           Foreign.Ptr              (IntPtr)
import qualified Std.Data.Builder.Numeric as B
import           Std.Data.Parser.Base     (Parser, (<?>))
import qualified Std.Data.Parser.Base     as P
import qualified Std.Data.Vector.Base     as V
import qualified Std.Data.Vector.Extra    as V

#define WORD64_MAX_DIGITS_LEN 18

#define PLUS     43
#define MINUS    45
#define DOT      46
#define LITTLE_E 101
#define BIG_E    69
#define C_0 48

-- | Parse and decode an unsigned hex number.  The hex digits
-- @\'a\'@ through @\'f\'@ may be upper or lower case.
--
-- This parser does not accept a leading @\"0x\"@ string, and consider
-- sign bit part of the binary hex nibbles, i.e.
-- 'parse hex "0xFF" == Right (-1 :: Int8)'
--
hex :: (Integral a, Bits a) => Parser a
{-# INLINE hex #-}
{-# SPECIALIZE INLINE hex :: Parser Int    #-}
{-# SPECIALIZE INLINE hex :: Parser Int64  #-}
{-# SPECIALIZE INLINE hex :: Parser Int32  #-}
{-# SPECIALIZE INLINE hex :: Parser Int16  #-}
{-# SPECIALIZE INLINE hex :: Parser Int8   #-}
{-# SPECIALIZE INLINE hex :: Parser Word   #-}
{-# SPECIALIZE INLINE hex :: Parser Word64 #-}
{-# SPECIALIZE INLINE hex :: Parser Word32 #-}
{-# SPECIALIZE INLINE hex :: Parser Word16 #-}
{-# SPECIALIZE INLINE hex :: Parser Word8  #-}
{-# SPECIALIZE INLINE hex :: Parser Integer #-}
{-# SPECIALIZE INLINE hex :: Parser IntPtr #-}
hex = "Std.Data.Parser.Numeric.hex" <?> hexLoop 0 <$> P.takeWhile1 isHexDigit

-- | decode hex digits sequence within an array.
hexLoop :: (Integral a, Bits a)
        => a    -- ^ accumulator, usually start from 0
        -> V.Bytes
        -> a
{-# INLINE hexLoop #-}
hexLoop = V.foldl' step
  where
    step a w = a `unsafeShiftL` 4 + fromIntegral (w2iHex w)
    w2iHex w
        | w <= 57   = w - 48
        | w <= 70   = w - 55
        | w <= 102  = w - 87

-- | A fast digit predicate.
isHexDigit :: Word8 -> Bool
{-# INLINE isHexDigit #-}
isHexDigit w = w - 48 <= 9 || w - 65 <= 5 || w - 97 <= 5

-- | Parse and decode an unsigned decimal number.
uint :: (Integral a) => Parser a
{-# INLINE uint #-}
{-# SPECIALIZE INLINE uint :: Parser Int    #-}
{-# SPECIALIZE INLINE uint :: Parser Int64  #-}
{-# SPECIALIZE INLINE uint :: Parser Int32  #-}
{-# SPECIALIZE INLINE uint :: Parser Int16  #-}
{-# SPECIALIZE INLINE uint :: Parser Int8   #-}
{-# SPECIALIZE INLINE uint :: Parser Word   #-}
{-# SPECIALIZE INLINE uint :: Parser Word64 #-}
{-# SPECIALIZE INLINE uint :: Parser Word32 #-}
{-# SPECIALIZE INLINE uint :: Parser Word16 #-}
{-# SPECIALIZE INLINE uint :: Parser Word8  #-}
{-# SPECIALIZE INLINE uint :: Parser Integer #-}
uint = "Std.Data.Parser.Numeric.uint" <?> decLoop 0 <$> P.takeWhile1 isDigit

-- | decode digits sequence within an array.
decLoop :: Integral a
        => a    -- ^ accumulator, usually start from 0
        -> V.Bytes
        -> a
{-# INLINE decLoop #-}
decLoop = V.foldl' step
  where step a w = a * 10 + fromIntegral (w - 48)

-- | decode digits sequence within an array.
--
-- A fast version to decode 'Integer' using machine word as much as possible.
decLoopIntegerFast :: V.Bytes -> Integer
{-# INLINE decLoopIntegerFast #-}
decLoopIntegerFast bs
    | V.length bs <= WORD64_MAX_DIGITS_LEN = fromIntegral (decLoop @Word64 0 bs)
    | otherwise                            = decLoop @Integer 0 bs

-- | A fast digit predicate.
isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
{-# INLINE isDigit #-}

-- | Parse a decimal number with an optional leading @\'+\'@ or @\'-\'@ sign
-- character.
int :: (Integral a) => Parser a
{-# INLINE int #-}
{-# SPECIALIZE INLINE int :: Parser Int    #-}
{-# SPECIALIZE INLINE int :: Parser Int64  #-}
{-# SPECIALIZE INLINE int :: Parser Int32  #-}
{-# SPECIALIZE INLINE int :: Parser Int16  #-}
{-# SPECIALIZE INLINE int :: Parser Int8   #-}
{-# SPECIALIZE INLINE int :: Parser Word   #-}
{-# SPECIALIZE INLINE int :: Parser Word64 #-}
{-# SPECIALIZE INLINE int :: Parser Word32 #-}
{-# SPECIALIZE INLINE int :: Parser Word16 #-}
{-# SPECIALIZE INLINE int :: Parser Word8  #-}
{-# SPECIALIZE INLINE int :: Parser Integer #-}
int = "Std.Data.Parser.Numeric.int" <?> do
    w <- P.peek
    if w == MINUS
    then P.skipWord8 *> (negate <$> uint')
    else if w == PLUS then P.skipWord8 *> uint' else uint'
  where
    -- strip uint's message
    uint' = decLoop 0 <$> P.takeWhile1 isDigit

-- | Parse a rational number.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
-- /Note/: this parser is not safe for use with inputs from untrusted
-- sources.  An input with a suitably large exponent such as
-- @"1e1000000000"@ will cause a huge 'Integer' to be allocated,
-- resulting in what is effectively a denial-of-service attack.
--
-- In most cases, it is better to use 'double' or 'scientific'
-- instead.
--
rational :: (Fractional a) => Parser a
{-# INLINE rational #-}
rational = "Std.Data.Parser.Numeric.rational" <?> scientificallyInternal realToFrac

-- | Parse a rational number and round to 'Double'.
--
-- This parser accepts an optional leading sign character, followed by
-- at least one decimal digit.  The syntax similar to that accepted by
-- the 'read' function, with the exception that a trailing @\'.\'@ or
-- @\'e\'@ /not/ followed by a number is not consumed.
--
-- Examples with behaviour identical to 'read':
--
-- >parse_ double "3"     == ("", Right 3.0)
-- >parse_ double "3.1"   == ("", Right 3.1)
-- >parse_ double "3e4"   == ("", Right 30000.0)
-- >parse_ double "3.1e4" == ("", Right 31000.0)
--
-- >parse_ double ".3"    == (".3", Left ParserError)
-- >parse_ double "e3"    == ("e3", Left ParserError)
--
-- Examples of differences from 'read':
--
-- >parse_ double "3.foo" == (".foo", Right 3.0)
-- >parse_ double "3e"    == ("e",    Right 3.0)
-- >parse_ double "-3e"   == ("e",    Right -3.0)
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
--
double :: Parser Double
{-# INLINE double #-}
double = "Std.Data.Parser.Numeric.double" <?> scientificallyInternal Sci.toRealFloat

-- | Parse a rational number and round to 'Float'.
--
-- Single precision version of 'double'.
float :: Parser Float
{-# INLINE float #-}
float = "Std.Data.Parser.Numeric.float" <?> scientificallyInternal Sci.toRealFloat

-- | Parse a scientific number.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
scientific :: Parser Sci.Scientific
{-# INLINE scientific #-}
scientific = "Std.Data.Parser.Numeric.scientific" <?> scientificallyInternal id

-- | Parse a scientific number and convert to result using a user supply function.
--
-- The syntax accepted by this parser is the same as for 'double'.
scientifically :: (Sci.Scientific -> a) -> Parser a
{-# INLINE scientifically #-}
scientifically h = "Std.Data.Parser.Numeric.scientifically" <?> scientificallyInternal h

-- | Strip message version.
scientificallyInternal :: (Sci.Scientific -> a) -> Parser a
{-# INLINE scientificallyInternal #-}
scientificallyInternal h = do
    !sign <- P.peek
    when (sign == PLUS || sign == MINUS) (P.skipWord8)
    !intPart <- P.takeWhile1 isDigit
    -- backtrack here is neccessary to avoid eating extra dot or e
    -- attoparsec is doing it wrong here: https://github.com/bos/attoparsec/issues/112
    !sci <- (do
        -- during number parsing we want to use machine word as much as possible
        -- so as long as range permit, we use Word64 instead of final Integer
        !fracPart <- P.word8 DOT *> P.takeWhile1 isDigit
        let !ilen = V.length intPart
            !flen = V.length fracPart
            !base =
                if ilen + flen <= WORD64_MAX_DIGITS_LEN
                then fromIntegral (decLoop @Word64 (decLoop @Word64 0 intPart) fracPart)
                else
                    let i = decLoopIntegerFast intPart
                        f = decLoopIntegerFast fracPart
                    in i * 10 ^ flen + f
        parseE base flen) <|> (parseE (decLoopIntegerFast intPart) 0)

    pure $! if sign /= MINUS then h sci else h (negate sci)
  where
    {-# INLINE parseE #-}
    parseE c e =
        (do _ <- P.satisfy (\w -> w ==  LITTLE_E || w == BIG_E)
            Sci.scientific c . subtract e <$> int) <|> pure (Sci.scientific c (negate e))

--------------------------------------------------------------------------------

-- | Parse a rational number.
--
-- The syntax accepted by this parser is the same as for 'double''.
--
-- /Note/: this parser is not safe for use with inputs from untrusted
-- sources.  An input with a suitably large exponent such as
-- @"1e1000000000"@ will cause a huge 'Integer' to be allocated,
-- resulting in what is effectively a denial-of-service attack.
--
-- In most cases, it is better to use 'double'' or 'scientific''
-- instead.
--
rational' :: (Fractional a) => Parser a
{-# INLINE rational' #-}
rational' = "Std.Data.Parser.Numeric.rational'" <?> scientificallyInternal' realToFrac

-- | More strict number parsing(rfc8259).
--
-- 'scientific' support parse @2314.@ and @21321exyz@ without eating extra dot or @e@ via
-- backtrack, this is not allowed in some strict grammer such as JSON, so we make an
-- non-backtrack strict number parser separately using LL(1) lookahead. This parser also
-- agree with 'read' on extra dot or e handling:
--
-- >parse_ double "3.foo" == Left ParseError
-- >parse_ double "3e"    == Left ParseError
--
-- Leading zeros or @+@ sign is also not allowed:
--
-- >parse_ double "+3.14" == Left ParseError
-- >parse_ double "0014" == Left ParseError
--
-- If you have a similar grammer, you can use this parser to save considerable time.
--
-- @
--      number = [ minus ] int [ frac ] [ exp ]
--      decimal-point = %x2E       ; .
--      digit1-9 = %x31-39         ; 1-9
--      e = %x65 / %x45            ; e E
--      exp = e [ minus / plus ] 1*DIGIT
--      frac = decimal-point 1*DIGIT
-- @
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
-- reference: https://tools.ietf.org/html/rfc8259#section-6
double' :: Parser Double
{-# INLINE double' #-}
double' = "Std.Data.Parser.Numeric.double'" <?> scientificallyInternal' Sci.toRealFloat

-- | Parse a rational number and round to 'Float' using stricter grammer.
--
-- Single precision version of 'double''.
float' :: Parser Float
{-# INLINE float' #-}
float' = "Std.Data.Parser.Numeric.float'" <?> scientificallyInternal' Sci.toRealFloat

-- | Parse a scientific number.
--
-- The syntax accepted by this parser is the same as for 'double''.
scientific' :: Parser Sci.Scientific
{-# INLINE scientific' #-}
scientific' = "Std.Data.Parser.Numeric.scientific'" <?> scientificallyInternal' id

-- | Parse a scientific number and convert to result using a user supply function.
--
-- The syntax accepted by this parser is the same as for 'double''.
scientifically' :: (Sci.Scientific -> a) -> P.Parser a
{-# INLINE scientifically' #-}
scientifically' h = "Std.Data.Parser.Numeric.scientifically'" <?> scientificallyInternal' h

-- | Strip message version of scientifically'.
scientificallyInternal' :: (Sci.Scientific -> a) -> P.Parser a
{-# INLINE scientificallyInternal' #-}
scientificallyInternal' h = do
    !sign <- P.peek
    when (sign == MINUS) (P.skipWord8) -- no leading plus is allowed
    !intPart <- P.takeWhile1 isDigit
    when (V.length intPart > 1 && V.head intPart == C_0) (fail "leading zeros are not allowed")
    mdot <- P.peekMaybe
    !sci <- case mdot of
        Just DOT -> do
            !fracPart <- P.skipWord8 *> P.takeWhile1 isDigit
            -- during number parsing we want to use machine word as much as possible
            -- so as long as range permit, we use Word64 instead of final Integer
            let !ilen = V.length intPart
                !flen = V.length fracPart
                !base =
                    if ilen + flen <= WORD64_MAX_DIGITS_LEN
                    then fromIntegral (decLoop @Word64 (decLoop @Word64 0 intPart) fracPart)
                    else
                        let i = decLoopIntegerFast intPart
                            f = decLoopIntegerFast fracPart
                        in i * 10 ^ flen + f
            parseE base flen
        _ -> parseE (decLoopIntegerFast intPart) 0
    pure $! if sign /= MINUS then h sci else h (negate sci)
  where
    {-# INLINE parseE #-}
    parseE !c !e = do
        me <- P.peekMaybe
        e' <- case me of
            Just ec | ec == LITTLE_E || ec == BIG_E -> P.skipWord8 *> int
            _ -> pure 0
        pure $! Sci.scientific c (e' - e)

--------------------------------------------------------------------------------

floatToScientific :: Float -> Sci.Scientific
{-# INLINE floatToScientific #-}
floatToScientific rf | rf < 0    = -(fromFloatingDigits (B.grisu3_sp (-rf)))
                     | rf == 0   = 0
                     | otherwise = fromFloatingDigits (B.grisu3_sp rf)

doubleToScientific :: Double -> Sci.Scientific
{-# INLINE doubleToScientific #-}
doubleToScientific rf | rf < 0    = -(fromFloatingDigits (B.grisu3 (-rf)))
                      | rf == 0   = 0
                      | otherwise = fromFloatingDigits (B.grisu3 rf)

fromFloatingDigits :: ([Int], Int) -> Sci.Scientific
{-# INLINE fromFloatingDigits #-}
fromFloatingDigits (digits, e) = go digits 0 0
  where
    -- There's no way a float or double has more digits a 'Int64' can't handle
    go :: [Int] -> Int64 -> Int -> Sci.Scientific
    go []     !c !n = Sci.scientific (fromIntegral c) (e - n)
    go (d:ds) !c !n = go ds (c * 10 + fromIntegral d) (n + 1)
