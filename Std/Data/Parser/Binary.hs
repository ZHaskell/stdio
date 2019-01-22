{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
-- |
-- Module      :  Std.Data.Parser.Binary
-- Copyright   :  Bryan O'Sullivan 2007-2015, Winterland 2016
-- License     :  BSD3
--
-- Maintainer  :  drkoster@qq.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing for 'V.Bytes'.
--
module Std.Data.Parser.Binary where

import           Control.Applicative
import           Control.Monad
import           Data.Word
import           Std.Data.Parser.Base
import qualified Std.Data.Vector.Base     as V
import qualified Std.Data.Vector.Extra    as V
import qualified Data.Primitive.PrimArray as A
import           Prelude                  hiding (takeWhile)

--------------------------------------------------------------------------------

-- | Match any byte, to perform lookahead. Returns 'Nothing' if end of
-- input has been reached. Does not consume any input.
--
peekMaybe :: Parser (Maybe Word8)
peekMaybe = do
    e <- isEmpty
    if e then return Nothing
         else Just <$> peek
{-# INLINE peekMaybe #-}

-- | Match any byte, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
--
peek :: Parser Word8
peek = do
    ensureN 1
    bs <- get
    return (V.unsafeHead bs)
{-# INLINE peek #-}

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
--
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p = do
    ensureN 1
    bs <- get
    let w = V.unsafeHead bs
    if p w then put (V.unsafeTail bs) >> return w
           else fail "satisfy"
{-# INLINE satisfy #-}

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
--
satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Parser a
satisfyWith f p = do
    ensureN 1
    bs <- get
    let w = V.unsafeHead bs
        r = f w
    if p r then put (V.unsafeTail bs) >> return r
           else fail "satisfyWith"
{-# INLINE satisfyWith #-}

-- | Match a specific byte.
--
word8 :: Word8 -> Parser ()
word8 c = do
    ensureN 1
    bs <- get
    let w = V.unsafeHead bs
    if c == w then put (V.unsafeTail bs)
              else fail "word8"
{-# INLINE word8 #-}

-- | Match any byte.
--
anyWord8 :: Parser Word8
anyWord8 = decodePrim

-- | The parser @skipWord8 p@ succeeds for any byte for which the predicate @p@ returns 'True'.
--
skipWord8 :: (Word8 -> Bool) -> Parser ()
skipWord8 p = do
    ensureN 1
    bs <- get
    let w = V.unsafeHead bs
    if p w then put (V.unsafeTail bs)
              else fail "skip"
{-# INLINE skipWord8 #-}

--------------------------------------------------------------------------------

-- | This is a faster version of 'skip' for small N (smaller than chunk size).
--
skipN :: Int -> Parser ()
skipN n = do
    bs <- get
    let l = V.length bs
    if l >= n then put (V.unsafeDrop n bs)
              else put V.empty >> skipN (l - n)
{-# INLINE skipN #-}

-- | Consume input as long as the predicate returns 'False' or reach the end of input,
-- and return the consumed input.
--
takeTill :: (Word8 -> Bool) -> Parser V.Bytes
takeTill p = do
    bs <- get
    let (want, rest) = V.break p bs
    put rest
    if V.null rest then V.concat . reverse <$> go [want] else return want
  where
    go acc = do
        e <- isEmpty -- isEmpty will draw input here
        if e
        then return acc
        else do
            bs <- get
            let (want, rest) = V.break p bs
                acc' = want : acc
            put rest
            if V.null rest then go acc' else return acc'
{-# INLINE takeTill #-}

-- | Consume input as long as the predicate returns 'True' or reach the end of input,
-- and return the consumed input.
--
takeWhile :: (Word8 -> Bool) -> Parser V.Bytes
takeWhile p = do
    bs <- get
    let (want, rest) = V.span p bs
    put rest
    if V.null rest then V.concat . reverse <$> go [want] else return want
  where
    go acc = do
        e <- isEmpty
        if e
        then return acc
        else do
            bs <- get
            let (want, rest) = V.span p bs
                acc' = want : acc
            put rest
            if V.null rest then go acc' else return acc'
{-# INLINE takeWhile #-}

-- | Similar to 'takeWhile', but requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or reach the end of input
--
takeWhile1 :: (Word8 -> Bool) -> Parser V.Bytes
takeWhile1 p = do
    bs <- takeWhile p
    if V.null bs then fail "takeWhile1" else return bs
{-# INLINE takeWhile1 #-}

-- | Skip past input for as long as the predicate returns 'True'.
--
skipWhile :: (Word8 -> Bool) -> Parser ()
skipWhile p = do
    bs <- get
    let rest = V.dropWhile p bs
    put rest
    when (V.null rest) go
  where
    go = do
        e <- isEmpty
        unless e $ do
            bs <- get
            let rest = V.dropWhile p bs
            put rest
            when (V.null rest) go
{-# INLINE skipWhile #-}

-- | Skip over white space using 'isSpace'.
--
skipSpaces :: Parser ()
skipSpaces = skipWhile isSpace
{-# INLINE skipSpaces #-}

-- | @string s@ parses a sequence of bytes that identically match @s@.
--
string :: V.Bytes -> Parser ()
string bs = do
    let l = V.length bs
    bs' <- get
    if l <= V.length bs'              -- current chunk is enough
    then if V.unsafeTake l bs' == bs
        then put (V.unsafeDrop l bs')
        else fail "string"
    else do
        ensureN l
        bs'' <- get
        if V.unsafeTake l bs'' == bs
        then put (V.unsafeDrop l bs'')
        else fail "string"
{-# INLINE string #-}


--------------------------------------------------------------------------------

-- | Fast 'Word8' predicate for matching ASCII space characters
--
-- >isSpace w = w == 32 || w - 9 <= 4
--
isSpace :: Word8 -> Bool
isSpace w = w == 32 || w - 9 <= 4
{-# INLINE isSpace #-}

-- | Decimal digit predicate.
--
isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
{-# INLINE isDigit #-}

-- | Hex digit predicate.
--
isHexDigit :: Word8 -> Bool
isHexDigit w = (w >= 48 && w <= 57) || (w >= 97 && w <= 102) || (w >= 65 && w <= 70)
{-# INLINE isHexDigit #-}

-- | A predicate that matches either a space @\' \'@ or horizontal tab
-- @\'\\t\'@ character.
--
isHorizontalSpace :: Word8 -> Bool
isHorizontalSpace w = w == 32 || w == 9
{-# INLINE isHorizontalSpace #-}

-- | A predicate that matches either a carriage return @\'\\r\'@ or
-- newline @\'\\n\'@ character.
--
isEndOfLine :: Word8 -> Bool
isEndOfLine w = w == 13 || w == 10
{-# INLINE isEndOfLine #-}

--------------------------------------------------------------------------------

-- | Match either a single newline byte @\'\\n\'@, or a carriage
-- return followed by a newline byte @\"\\r\\n\"@.
endOfLine :: Parser ()
endOfLine = do
    w <- decodePrim :: Parser Word8
    case w of
        10 -> return ()
        13 -> word8 10
        _  -> fail "endOfLine"
{-# INLINE endOfLine #-}
