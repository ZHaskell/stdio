{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}

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
  , isHexDigit
  , isDigit
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Int
import qualified Data.Primitive.PrimArray  as A
import qualified Data.Scientific          as Sci
import           Data.Word
import           Foreign.Ptr              (IntPtr)
import           Std.Data.Parser.Base     (Parser)
import qualified Std.Data.Parser.Base     as P
import qualified Std.Data.Vector.Base     as V
import qualified Std.Data.Vector.Extra    as V
import           Std.IO.Exception

#define WORD64_MAX_DIGITS_LEN 18

#define PLUS     43
#define MINUS    45
#define DOT      46
#define LITTLE_E 101
#define BIG_E    69

-- | Parse and decode an unsigned hex number.  The hex digits
-- @\'a\'@ through @\'f\'@ may be upper or lower case.
--
-- This parser does not accept a leading @\"0x\"@ string, and consider
-- sign bit part of the binary hex nibbles, i.e.
-- 'parse hex "0xFF" == Right (-1 :: Int8)'
--
hex :: (HasCallStack, Integral a, Bits a) => Parser a
{-# INLINABLE hex #-}
{-# SPECIALIZE INLINE hex :: HasCallStack => Parser Int    #-}
{-# SPECIALIZE INLINE hex :: HasCallStack => Parser Int64  #-}
{-# SPECIALIZE INLINE hex :: HasCallStack => Parser Int32  #-}
{-# SPECIALIZE INLINE hex :: HasCallStack => Parser Int16  #-}
{-# SPECIALIZE INLINE hex :: HasCallStack => Parser Int8   #-}
{-# SPECIALIZE INLINE hex :: HasCallStack => Parser Word   #-}
{-# SPECIALIZE INLINE hex :: HasCallStack => Parser Word64 #-}
{-# SPECIALIZE INLINE hex :: HasCallStack => Parser Word32 #-}
{-# SPECIALIZE INLINE hex :: HasCallStack => Parser Word16 #-}
{-# SPECIALIZE INLINE hex :: HasCallStack => Parser Word8  #-}
{-# SPECIALIZE INLINE hex :: HasCallStack => Parser Integer #-}
{-# SPECIALIZE INLINE hex :: HasCallStack => Parser IntPtr #-}
hex = hexLoop 0 <$> P.takeWhile1 isHexDigit

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
uint :: (HasCallStack, Integral a) => Parser a
{-# INLINABLE uint #-}
{-# SPECIALIZE uint :: HasCallStack => Parser Int    #-}
{-# SPECIALIZE uint :: HasCallStack => Parser Int64  #-}
{-# SPECIALIZE uint :: HasCallStack => Parser Int32  #-}
{-# SPECIALIZE uint :: HasCallStack => Parser Int16  #-}
{-# SPECIALIZE uint :: HasCallStack => Parser Int8   #-}
{-# SPECIALIZE uint :: HasCallStack => Parser Word   #-}
{-# SPECIALIZE uint :: HasCallStack => Parser Word64 #-}
{-# SPECIALIZE uint :: HasCallStack => Parser Word32 #-}
{-# SPECIALIZE uint :: HasCallStack => Parser Word16 #-}
{-# SPECIALIZE uint :: HasCallStack => Parser Word8  #-}
{-# SPECIALIZE uint :: HasCallStack => Parser Integer #-}
uint = decLoop 0 <$> P.takeWhile1 isDigit

-- | decode digits sequence within an array.
decLoop :: Integral a
        => a    -- ^ accumulator, usually start from 0
        -> V.Bytes
        -> a
{-# INLINE decLoop #-}
decLoop a bs@(V.PrimVector arr s l) = V.foldl' step a bs
  where step a w = a * 10 + fromIntegral (w - 48)


-- | A fast digit predicate.
isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
{-# INLINE isDigit #-}

-- | Parse a decimal number with an optional leading @\'+\'@ or @\'-\'@ sign
-- character.
int :: (HasCallStack, Integral a) => Parser a
{-# INLINABLE int #-}
{-# SPECIALIZE int :: HasCallStack => Parser Int    #-}
{-# SPECIALIZE int :: HasCallStack => Parser Int64  #-}
{-# SPECIALIZE int :: HasCallStack => Parser Int32  #-}
{-# SPECIALIZE int :: HasCallStack => Parser Int16  #-}
{-# SPECIALIZE int :: HasCallStack => Parser Int8   #-}
{-# SPECIALIZE int :: HasCallStack => Parser Word   #-}
{-# SPECIALIZE int :: HasCallStack => Parser Word64 #-}
{-# SPECIALIZE int :: HasCallStack => Parser Word32 #-}
{-# SPECIALIZE int :: HasCallStack => Parser Word16 #-}
{-# SPECIALIZE int :: HasCallStack => Parser Word8  #-}
{-# SPECIALIZE int :: HasCallStack => Parser Integer #-}
int = do
    w <- P.peek
    if w == MINUS
    then P.skip 1 *> (negate <$> uint)
    else if w == PLUS then P.skip 1 *> uint else uint

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
rational :: (HasCallStack, Fractional a) => Parser a
{-# INLINE rational #-}
rational = scientifically realToFrac

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
double :: HasCallStack => Parser Double
{-# INLINE double #-}
double = scientifically Sci.toRealFloat

-- | Parse a rational number and round to 'Float'.
--
-- Single precision version of 'double'.
float :: HasCallStack => Parser Float
{-# INLINE float #-}
float = scientifically Sci.toRealFloat

-- | Parse a scientific number.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
scientific :: HasCallStack => Parser Sci.Scientific
{-# INLINE scientific #-}
scientific = scientifically id

-- | Parse a scientific number and convert to result using a user supply function.
--
-- The syntax accepted by this parser is the same as for 'double'.
scientifically :: HasCallStack => (Sci.Scientific -> a) -> Parser a
{-# INLINE scientifically #-}
scientifically h = do
    !sign <- P.peek
    when (sign == PLUS || sign == MINUS) (P.skip 1)
    !intPart <- P.takeWhile1 isDigit
    -- backtrace here is neccessary to avoid eating dot or e
    -- attoparsec is doing it wrong here: https://github.com/bos/attoparsec/issues/112
    !sci <- (do
        !fracPart <- P.word8 DOT *> P.takeWhile1 isDigit
        let !ilen = V.length intPart
            !flen = V.length fracPart
            !base =
                if ilen + flen <= WORD64_MAX_DIGITS_LEN
                then fromIntegral (decLoop @Word64 (decLoop @Word64 0 intPart) fracPart)
                else
                    let int =
                            if ilen <= WORD64_MAX_DIGITS_LEN
                            then fromIntegral (decLoop @Word64 0 intPart)
                            else decLoop @Integer 0 intPart
                        frac =
                            if flen <= WORD64_MAX_DIGITS_LEN
                            then fromIntegral (decLoop @Word64 0 fracPart)
                            else decLoop @Integer 0 fracPart
                    in int * 10 ^ flen + frac
        parseE base flen) <|> (do
        let !ilen = V.length intPart
            !base =
                if ilen <= WORD64_MAX_DIGITS_LEN
                then fromIntegral (decLoop @Word64 0 intPart)
                else decLoop @Integer 0 intPart
        parseE base 0)

    return $! if sign /= MINUS then h sci else h (negate sci)
  where
    {-# INLINE parseE #-}
    parseE c e =
        (do _ <- P.satisfy (\w -> w ==  LITTLE_E || w == BIG_E)
            Sci.scientific c . subtract e <$> int) <|> return (Sci.scientific c (negate e))

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
rational' :: (HasCallStack, Fractional a) => Parser a
{-# INLINE rational' #-}
rational' = scientifically' realToFrac

-- | More strict number parsing(rfc8259).
--
-- 'scientific' support parse @2314.@ and @21321exyz@ without eating extra dot or @e@ via
-- backtrace, this is not allowed in some strict grammer such as JSON, so we make an
-- non-backtrace strict number parser separately using LL(1) lookahead. This parser also
-- agree with 'read' on extra dot or e handling:
--
-- >parse_ double "3.foo" == Left ParseError
-- >parse_ double "3e"    == Left ParseError
--
-- The leading @+@ sign is also not allowed:
--
-- >parse_ double "+3.14" == Left ParseError
--
-- If you have a similar grammer, you can use this parse to save considerable time.
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
double' :: HasCallStack => Parser Double
{-# INLINE double' #-}
double' = scientifically' Sci.toRealFloat

-- | Parse a rational number and round to 'Float' using stricter grammer.
--
-- Single precision version of 'double''.
float' :: HasCallStack => Parser Float
{-# INLINE float' #-}
float' = scientifically' Sci.toRealFloat

-- | Parse a scientific number.
--
-- The syntax accepted by this parser is the same as for 'double''.
scientific' :: HasCallStack => Parser Sci.Scientific
{-# INLINE scientific' #-}
scientific' = scientifically' id

-- | Parse a scientific number and convert to result using a user supply function.
--
-- The syntax accepted by this parser is the same as for 'double''.
scientifically' :: HasCallStack => (Sci.Scientific -> a) -> P.Parser a
{-# INLINE scientifically' #-}
scientifically' h = do
    sign <- P.peek
    when (sign == MINUS) (P.skip 1) -- no leading plus is allowed
    !intPart <- P.takeWhile1 isDigit
    mdot <- P.peekMaybe
    !sci <- case mdot of
        Just DOT -> do
            !fracPart <- P.skip 1 *> P.takeWhile1 isDigit
            -- during number parsing we want to use machine word as much as possible
            -- so as long as range permit, we use Word64 instead of final Integer
            let !ilen = V.length intPart
                !flen = V.length fracPart
                !base =
                    if ilen + flen <= WORD64_MAX_DIGITS_LEN
                    then fromIntegral (decLoop @Word64 (decLoop @Word64 0 intPart) fracPart)
                    else
                        let int =
                                if ilen <= WORD64_MAX_DIGITS_LEN
                                then fromIntegral (decLoop @Word64 0 intPart)
                                else decLoop @Integer 0 intPart
                            frac =
                                if flen <= WORD64_MAX_DIGITS_LEN
                                then fromIntegral (decLoop @Word64 0 fracPart)
                                else decLoop @Integer 0 fracPart
                        in int * 10 ^ flen + frac
            parseE base flen
        _ ->
            let !ilen = V.length intPart
                !base =
                    if ilen <= WORD64_MAX_DIGITS_LEN
                    then fromIntegral (decLoop @Word64 0 intPart)
                    else decLoop @Integer 0 intPart
            in parseE base 0
    return $! if sign /= MINUS then h sci else h (negate sci)
  where
    {-# INLINE parseE #-}
    parseE !c !exp = do
        me <- P.peekMaybe
        exp' <- case me of
            Just e | e == LITTLE_E || e == BIG_E -> P.skip 1 *> int
            _ -> return 0
        return $! Sci.scientific c (exp' - exp)
