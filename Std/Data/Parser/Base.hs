{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Std.Data.Parser.Base
Description : Efficient deserialization/parse.
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide a simple resumable 'Parser', which is suitable for binary protocol and simple textual protocol parsing.

You can use 'Alternative' instance to do backtracking, each branch will either succeed and may consume some input, or fail without consume anything. It's recommend to use 'peek' or 'peekMaybe' to avoid backtracking if possible to get high performance.

To help debugging, please prepend 'HasCallStack' constraint in your parsers, we don't provide manual labeling. The default output format for 'ParseError' only shows combinators' unqualified name for simplicity:

@
    >parse int "foo"
    ([102,111,111],Left int.uint.takeWhile1: unsatisfied byte)
    -- It's easy to see we're trying to match a leading sign or digit here
@

-}

module Std.Data.Parser.Base
  ( -- * Parser types
    Result(..)
  , ParseError
  , ParseStep
  , Parser(..)
  , HasCallStack
  , failWithStack
    -- * Running a parser
  , parse, parse_, parseChunk, parseChunks, finishParsing
  , runAndKeepTrack, match
    -- * Basic parsers
  , ensureN, endOfInput
    -- * Primitive decoders
  , decodePrim, decodePrimLE, decodePrimBE
    -- * More parsers
  , scan, scanChunks, peekMaybe, peek, satisfy, satisfyWith
  , word8, char8, anyWord8, endOfLine, skip, skipWhile, skipSpaces
  , take, takeTill, takeWhile, takeWhile1, bytes, bytesCI
  , text
    -- * Misc
  , isSpace
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Fail                 as Fail
import qualified Data.CaseInsensitive               as CI
import qualified Data.Primitive.PrimArray           as A
import           Data.Int
import           Data.Typeable
import qualified Data.List                          as List
import           Data.Word
import           GHC.Types
import           Prelude                            hiding (take, takeWhile)
import           Std.Data.PrimArray.UnalignedAccess
import qualified Std.Data.Text.Base                 as T
import qualified Std.Data.Vector.Base               as V
import qualified Std.Data.Vector.Extra              as V
import           Std.IO.Exception
import           GHC.Stack


-- | Simple parsing result, that represent respectively:
--
-- * success: the remaining unparsed data and the parsed value
--
-- * failure: the remaining unparsed data and the error message
--
-- * partial: that need for more input data, supply empty bytes to indicate 'endOfInput'
--
data Result a
    = Success !V.Bytes a
    | Failure !V.Bytes ParseError
    | Partial (V.Bytes -> Result a)

data ParseError = ParseError
    { parserStack :: CallStack
    , parserError :: T.Text
    } deriving Typeable

-- | This instance is for testing so that @Eq a => Eq (Either ParseError a)@
instance Eq ParseError where
    (ParseError cs1 msg1) == (ParseError cs2 msg2) =
        (getCallStack cs1) == (getCallStack cs2) && msg1 == msg2

instance Show ParseError where
    show (ParseError stack err) =
        List.intercalate "." (List.reverse $ List.map fst (getCallStack stack)) ++ ": " ++ T.unpack err

instance Exception ParseError

instance Functor Result where
    fmap f (Success s a)   = Success s (f a)
    fmap f (Partial k)     = Partial (fmap f . k)
    fmap _ (Failure v e)   = Failure v e

instance Show a => Show (Result a) where
    show (Success _ a)    = "Success " ++ show a
    show (Partial _)      = "Partial _"
    show (Failure _ errs) = "Failure: " ++ show errs

type ParseStep r = V.Bytes -> Result r

-- | Simple CPSed parser
--
newtype Parser a = Parser { runParser :: forall r .  (a -> ParseStep r) -> ParseStep r }


-- It seems eta-expand one layer to ensure parser are saturated is helpful
instance Functor Parser where
    fmap f (Parser pa) = Parser (\ k inp -> pa (k . f) inp)
    {-# INLINE fmap #-}
    a <$ Parser pb = Parser (\ k inp -> pb (\ _ -> k a) inp)
    {-# INLINE (<$) #-}

instance Applicative Parser where
    pure x = Parser (\ k inp -> k x inp)
    {-# INLINE pure #-}
    Parser pf <*> Parser pa = Parser (\ k inp -> pf (\ f -> pa (k . f)) inp)
    {-# INLINE (<*>) #-}
    Parser pa *> Parser pb = Parser (\ k inp -> pa (\ _ -> pb k) inp)
    {-# INLINE (*>) #-}
    Parser pa <* Parser pb = Parser (\ k inp -> pa (\ x -> pb (\ _ -> k x)) inp)
    {-# INLINE (<*) #-}

instance Monad Parser where
    return = pure
    {-# INLINE return #-}
    Parser pa >>= f = Parser (\ k inp -> pa (\ a -> runParser (f a) k) inp)
    {-# INLINE (>>=) #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}
    fail = Fail.fail
    {-# INLINE fail #-}

instance Fail.MonadFail Parser where
    fail str = Parser (\ _ inp -> Failure inp (ParseError callStack (T.pack str)))
    {-# INLINE fail #-}

instance MonadPlus Parser where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance Alternative Parser where
    empty = Parser (\ _ input -> Failure input
        (ParseError callStack "Std.Data.Parser.Base(Alternative).empty"))
    {-# INLINE empty #-}
    f <|> g = do
        (r, bss) <- runAndKeepTrack f
        case r of
            Success input x     -> Parser (\ k _ -> k x input)
            Failure input' _    -> let !bs = V.concat (reverse bss)
                                   in Parser (\ k _ -> runParser g k bs)
            _                   -> error "Std.Data.Parser.Base: impossible"
    {-# INLINE (<|>) #-}

-- | Don't call fail if possible since it does not provide callStacks, use
-- 'failWithStack' instead.
failWithStack :: HasCallStack => T.Text -> Parser a
failWithStack msg = Parser (\ _ input ->
    Failure input (ParseError (popCallStack callStack) msg))    -- itself's stack is erased

-- | Parse the complete input, without resupplying
parse_ :: Parser a -> V.Bytes -> Either ParseError a
{-# INLINE parse_ #-}
parse_ (Parser p) input = snd $ finishParsing (p (flip Success) input)

-- | Parse the complete input, without resupplying, return the rest bytes
parse :: Parser a -> V.Bytes -> (V.Bytes, Either ParseError a)
{-# INLINE parse #-}
parse (Parser p) input = finishParsing (p (flip Success) input)

-- | Parse an input chunk
parseChunk :: Parser a -> V.Bytes -> Result a
{-# INLINE parseChunk #-}
parseChunk (Parser p) = p (flip Success)

-- | Finish parsing and fetch result, feed empty bytes if it's 'Partial' result.
finishParsing :: Result a -> (V.Bytes, Either ParseError a)
{-# INLINABLE finishParsing #-}
finishParsing r = case r of
    Success rest a    -> (rest, Right a)
    Failure rest errs -> (rest, Left errs)
    Partial f         -> finishParsing (f V.empty)

-- | Run a parser with an initial input string, and a monadic action
-- that can supply more input if needed.
--
-- Note, once the monadic action return empty bytes, parsers will stop drawing
-- more bytes (take it as 'endOfInput').
parseChunks :: Monad m => Parser a -> m V.Bytes -> V.Bytes -> m (V.Bytes, Either ParseError a)
{-# INLINABLE parseChunks #-}
parseChunks (Parser p) m input = go m (p (flip Success) input)
  where
    go m r = case r of
        Partial f -> do
            inp <- m
            if V.null inp
            then go (return V.empty) (f V.empty)
            else go m (f inp)
        Success rest a    -> return (rest, Right a)
        Failure rest errs -> return (rest, Left errs)

-- | Run a parser and keep track of all the input chunks it consumes.
-- Once it's finished, return the final result (always 'Success' or 'Failure') and
-- all consumed chunks.
--
runAndKeepTrack :: HasCallStack => Parser a -> Parser (Result a, [V.Bytes])
{-# INLINE runAndKeepTrack #-}
runAndKeepTrack (Parser pa) = Parser $ \ k0 input ->
    let r0 = pa (\ a input' -> Success input' a) input in go [input] r0 k0
  where
    go !acc r k0 = case r of
        Partial k        -> Partial (\ input -> go (input:acc) (k input) k0)
        Success input' _ -> k0 (r, reverse acc) input'
        Failure input' _ -> k0 (r, reverse acc) input'

-- | Return both the result of a parse and the portion of the input
-- that was consumed while it was being parsed.
match :: HasCallStack => Parser a -> Parser (V.Bytes, a)
{-# INLINE match #-}
match p = do
    (r, bss) <- runAndKeepTrack p
    Parser (\ k _ ->
        case r of
            Success input' r'  -> let !consumed = V.dropR (V.length input') (V.concat (reverse bss))
                                  in k (consumed , r') input'
            Failure input' err -> Failure input' err
            Partial k          -> error "Std.Data.Parser.Base: impossible")


-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'Partial'.
ensureN :: HasCallStack => Int -> Parser ()
{-# INLINE ensureN #-}
ensureN n0 = Parser $ \ ks input -> do
    let l = V.length input
    if l >= n0
    then ks () input
    else Partial (go n0 ks [input] l)
  where
    go !n0 ks acc !l = \ inp -> do
        let l' = V.length inp
        if l' == 0
        then Failure
            (V.concat (reverse (inp:acc)))
            (ParseError callStack "not enough bytes")
        else do
            let l'' = l + l'
            if l'' < n0
            then Partial (go n0 ks (inp:acc) l'')
            else do
                let !inp' = V.concat (reverse (inp:acc))
                ks () inp'

-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes.
endOfInput :: Parser Bool
{-# INLINE endOfInput #-}
endOfInput = Parser $ \ k inp ->
    if V.null inp
    then Partial (\ inp' -> k (V.null inp') inp')
    else k False inp

decodePrim :: forall a. (UnalignedAccess a, HasCallStack) => Parser a
{-# INLINE decodePrim #-}
{-# SPECIALIZE INLINE decodePrim :: Parser Word   #-}
{-# SPECIALIZE INLINE decodePrim :: Parser Word64 #-}
{-# SPECIALIZE INLINE decodePrim :: Parser Word32 #-}
{-# SPECIALIZE INLINE decodePrim :: Parser Word16 #-}
{-# SPECIALIZE INLINE decodePrim :: Parser Word8  #-}
{-# SPECIALIZE INLINE decodePrim :: Parser Int   #-}
{-# SPECIALIZE INLINE decodePrim :: Parser Int64 #-}
{-# SPECIALIZE INLINE decodePrim :: Parser Int32 #-}
{-# SPECIALIZE INLINE decodePrim :: Parser Int16 #-}
{-# SPECIALIZE INLINE decodePrim :: Parser Int8  #-}
decodePrim = do
    ensureN n
    Parser (\ k (V.PrimVector (A.PrimArray ba#) i@(I# i#) len) ->
        let !r = indexWord8ArrayAs ba# i#
        in k r (V.PrimVector (A.PrimArray ba#) (i+n) (len-n)))
  where
    n = (getUnalignedSize (unalignedSize :: UnalignedSize a))

decodePrimLE :: (UnalignedAccess (LE a), HasCallStack) => Parser a
{-# INLINE decodePrimLE #-}
{-# SPECIALIZE INLINE decodePrimLE :: Parser Word   #-}
{-# SPECIALIZE INLINE decodePrimLE :: Parser Word64 #-}
{-# SPECIALIZE INLINE decodePrimLE :: Parser Word32 #-}
{-# SPECIALIZE INLINE decodePrimLE :: Parser Word16 #-}
{-# SPECIALIZE INLINE decodePrimLE :: Parser Int   #-}
{-# SPECIALIZE INLINE decodePrimLE :: Parser Int64 #-}
{-# SPECIALIZE INLINE decodePrimLE :: Parser Int32 #-}
{-# SPECIALIZE INLINE decodePrimLE :: Parser Int16 #-}
decodePrimLE = getLE <$> decodePrim

decodePrimBE :: (UnalignedAccess (BE a), HasCallStack) => Parser a
{-# INLINE decodePrimBE #-}
{-# SPECIALIZE INLINE decodePrimBE :: Parser Word   #-}
{-# SPECIALIZE INLINE decodePrimBE :: Parser Word64 #-}
{-# SPECIALIZE INLINE decodePrimBE :: Parser Word32 #-}
{-# SPECIALIZE INLINE decodePrimBE :: Parser Word16 #-}
{-# SPECIALIZE INLINE decodePrimBE :: Parser Int   #-}
{-# SPECIALIZE INLINE decodePrimBE :: Parser Int64 #-}
{-# SPECIALIZE INLINE decodePrimBE :: Parser Int32 #-}
{-# SPECIALIZE INLINE decodePrimBE :: Parser Int16 #-}
decodePrimBE = getBE <$> decodePrim

-- | A stateful scanner.  The predicate consumes and transforms a
-- state argument, and each transformed state is passed to successive
-- invocations of the predicate on each byte of the input until one
-- returns 'Nothing' or the input ends.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'Nothing' on the first byte of input.
--
scan :: s -> (s -> Word8 -> Maybe s) -> Parser (V.Bytes, s)
{-# INLINE scan #-}
scan s0 f = scanChunks s0 f'
  where
    f' st (V.PrimVector arr off l) =
        let !end = off + l
            go !st !i
                | i < end = do
                    let !w = A.indexPrimArray arr i
                    case f st w of
                        Just st' -> go st' (i+1)
                        _        ->
                            let !len1 = i - off
                                !len2 = end - off
                            in Right (V.PrimVector arr off len1, V.PrimVector arr i len2, st)
                | otherwise = Left st
        in go s0 off

-- | Similar to 'scan', but working on 'V.Bytes' chunks, The predicate
-- consumes a 'V.Bytes' chunk and transforms a state argument,
-- and each transformed state is passed to successive invocations of
-- the predicate on each chunk of the input until one chunk got splited to
-- @Right (V.Bytes, V.Bytes)@ or the input ends.
--
scanChunks :: s -> (s -> V.Bytes -> Either s (V.Bytes, V.Bytes, s)) -> Parser (V.Bytes, s)
{-# INLINE scanChunks #-}
scanChunks s consume = Parser (\ k inp ->
    case consume s inp of
        Right (want, rest, s') -> k (want, s') rest
        Left s' -> Partial (go consume s' [inp] k))
  where
    go consume s acc k = \ inp ->
        if V.null inp
        then k (V.concat (reverse acc), s) inp
        else case consume s inp of
                Left s' -> do
                    let acc' = inp : acc
                    Partial (go consume s' acc' k)
                Right (want,rest,s') ->
                    k (V.concat (reverse (want:acc)), s') rest

--------------------------------------------------------------------------------

-- | Match any byte, to perform lookahead. Returns 'Nothing' if end of
-- input has been reached. Does not consume any input.
--
peekMaybe :: Parser (Maybe Word8)
{-# INLINE peekMaybe #-}
peekMaybe = do
    e <- endOfInput
    if e then return Nothing
         else Just <$> peek

-- | Match any byte, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
--
peek :: HasCallStack => Parser Word8
{-# INLINE peek #-}
peek = do
    ensureN 1
    Parser (\ k inp -> k (V.unsafeHead inp) inp)

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
--
satisfy :: HasCallStack => (Word8 -> Bool) -> Parser Word8
{-# INLINE satisfy #-}
satisfy p = do
    ensureN 1
    Parser (\ k inp ->
        let w = V.unsafeHead inp
        in if p w
            then k w (V.unsafeTail inp)
            else Failure inp (ParseError callStack "unsatisfied byte"))

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
--
satisfyWith :: HasCallStack => (Word8 -> a) -> (a -> Bool) -> Parser a
{-# INLINE satisfyWith #-}
satisfyWith f p = do
    ensureN 1
    Parser (\ k inp ->
        let w = f (V.unsafeHead inp)
        in if p w
            then k w (V.unsafeTail inp)
            else Failure inp (ParseError callStack "unsatisfied byte"))

-- | Match a specific byte.
--
word8 :: HasCallStack => Word8 -> Parser ()
{-# INLINE word8 #-}
word8 w' = do
    ensureN 1
    Parser (\ k inp ->
        let w = V.unsafeHead inp
        in if w == w'
            then k () (V.unsafeTail inp)
            else Failure inp (ParseError callStack "mismatch byte"))

-- | Match a specific 8bit char.
--
char8 :: HasCallStack => Char -> Parser ()
{-# INLINE char8 #-}
char8 = word8 . V.c2w

-- | Match any byte.
--
anyWord8 :: HasCallStack => Parser Word8
{-# INLINE anyWord8 #-}
anyWord8 = decodePrim

-- | Match either a single newline byte @\'\\n\'@, or a carriage
-- return followed by a newline byte @\"\\r\\n\"@.
endOfLine :: HasCallStack => Parser ()
{-# INLINE endOfLine #-}
endOfLine = do
    w <- decodePrim :: Parser Word8
    case w of
        10 -> return ()
        13 -> word8 10
        _  -> failWithStack "mismatch byte"

--------------------------------------------------------------------------------

-- | 'skip' N bytes.
--
skip :: HasCallStack => Int -> Parser ()
{-# INLINE skip #-}
skip n =
    Parser (\ k inp ->
        let l = V.length inp
        in if l >= n'
            then k () $! V.unsafeDrop n' inp
            else Partial (go k (n'-l)))
  where
    !n' = max n 0
    go k !n = \ inp ->
        let l = V.length inp
        in if l >= n'
            then k () $! V.unsafeDrop n' inp
            else if l == 0
                then Failure inp (ParseError callStack "not enough bytes")
                else Partial (go k (n'-l))

-- | Skip past input for as long as the predicate returns 'True'.
--
skipWhile :: (Word8 -> Bool) -> Parser ()
{-# INLINE skipWhile #-}
skipWhile p =
    Parser (\ k inp ->
        let rest = V.dropWhile p inp
        in if V.null rest
            then Partial (go k p)
            else k () rest)
  where
    go k p = \ inp ->
        if V.null inp
        then k () inp
        else
            let !rest = V.dropWhile p inp
            in if V.null rest
                then Partial (go k p)
                else k () rest

-- | Skip over white space using 'isSpace'.
--
skipSpaces :: Parser ()
{-# INLINE skipSpaces #-}
skipSpaces = skipWhile isSpace

-- | @isSpace w = w == 32 || w - 9 <= 4 || w == 0xA0@
isSpace :: Word8 -> Bool
{-# INLINE isSpace #-}
isSpace w = w == 32 || w - 9 <= 4 || w == 0xA0

take :: HasCallStack => Int -> Parser V.Bytes
{-# INLINE take #-}
take n = do
    ensureN n' -- we use unsafe slice, guard negative n here
    Parser (\ k inp ->
        let !r = V.unsafeTake n' inp
            !inp' = V.unsafeDrop n' inp
        in k r inp')
  where !n' = max 0 n

-- | Consume input as long as the predicate returns 'False' or reach the end of input,
-- and return the consumed input.
--
takeTill :: (Word8 -> Bool) -> Parser V.Bytes
{-# INLINE takeTill #-}
takeTill p = Parser (\ k inp ->
    let (want, rest) = V.break p inp
    in if V.null rest
        then Partial (go p k [want])
        else k want rest)
  where
    go p k acc = \ inp ->
        if V.null inp
        then let !r = V.concat (reverse acc) in k r inp
        else
            let (want, rest) = V.break p inp
                acc' = want : acc
            in if V.null rest
                then Partial (go p k acc')
                else let !r = V.concat (reverse acc') in k r rest

-- | Consume input as long as the predicate returns 'True' or reach the end of input,
-- and return the consumed input.
--
takeWhile :: (Word8 -> Bool) -> Parser V.Bytes
{-# INLINE takeWhile #-}
takeWhile p = Parser (\ k inp ->
    let (want, rest) = V.span p inp
    in if V.null rest
        then Partial (go p k [want])
        else k want rest)
  where
    go p k acc = \ inp ->
        if V.null inp
        then let !r = V.concat (reverse acc) in k r inp
        else
            let (want, rest) = V.span p inp
                acc' = want : acc
            in if V.null rest
                then Partial (go p k acc')
                else let !r = V.concat (reverse acc') in k r rest

-- | Similar to 'takeWhile', but requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or reach the end of input
--
takeWhile1 :: HasCallStack => (Word8 -> Bool) -> Parser V.Bytes
{-# INLINE takeWhile1 #-}
takeWhile1 p = do
    bs <- takeWhile p
    if V.null bs then failWithStack "no satisfied byte" else return bs

-- | @bytes s@ parses a sequence of bytes that identically match @s@.
--
bytes :: HasCallStack => V.Bytes -> Parser ()
{-# INLINE bytes #-}
bytes bs = do
    let n = V.length bs
    ensureN n
    Parser (\ k inp ->
        if bs == V.unsafeTake n inp
        then k () $! V.unsafeDrop n inp
        else Failure inp (ParseError callStack "mismatch bytes"))


-- | Same as 'bytes' but ignoring case.
bytesCI :: HasCallStack => V.Bytes -> Parser ()
{-# INLINE bytesCI #-}
bytesCI bs = do
    let n = V.length bs
    ensureN n   -- casefold an ASCII string should not change it's length
    Parser (\ k inp ->
        if bs' == CI.foldCase (V.unsafeTake n inp)
        then k () $! V.unsafeDrop n inp
        else Failure inp (ParseError callStack "mismatch bytes"))
  where
    bs' = CI.foldCase bs

-- | @text s@ parses a sequence of UTF8 bytes that identically match @s@.
--
text :: HasCallStack => T.Text -> Parser ()
{-# INLINE text #-}
text (T.Text bs) = bytes bs
