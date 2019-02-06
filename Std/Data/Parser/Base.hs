{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Std.Data.Parser.Base
Description : Efficient deserialization/parse.
Copyright   : (c) Winterland, 2017-2019
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide a simple resumable 'Parser', which is suitable for binary protocol and simple textual protocol parsing.

You can use 'Alternative' instance to do backtracking, each branch will either succeed and may consume some input, or fail without consume anything. It's recommend to use 'peek' to avoid backtracking if possible to get high performance.

-}

module Std.Data.Parser.Base
  ( -- * Parser types
    Result(..)
  , ParseStep
  , Parser(..)
    -- * Running a parser
  , parse, parse', parseChunk, parseChunks, finishParsing
  , runAndKeepTrack
    -- * Basic parsers
  , ensureN, endOfInput
    -- * Primitive decoders
  , decodePrim, decodePrimLE, decodePrimBE
    -- * More parsers
  , scan, scanChunks, peekMaybe, peek, satisfy, satisfyWith
  , word8, anyWord8, endOfLine, skip, skipWhile, skipSpaces
  , take, takeTill, takeWhile, takeWhile1, bytes, bytesCI
  , text
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Fail                 as Fail
import qualified Data.CaseInsensitive               as CI
import qualified Data.Primitive.PrimArray           as A
import           Data.Int
import           Data.Word
import           Data.Word8                         (isSpace)
import           GHC.Types
import           Prelude                            hiding (take, takeWhile)
import           Std.Data.PrimArray.UnalignedAccess
import qualified Std.Data.Text.Base                 as T
import qualified Std.Data.Vector.Base               as V
import qualified Std.Data.Vector.Extra              as V

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
    | Failure !V.Bytes String
    | Partial (V.Bytes -> Result a)

instance Functor Result where
    fmap f (Success s a)   = Success s (f a)
    fmap _ (Failure s msg) = Failure s msg
    fmap f (Partial k)     = Partial (fmap f . k)

instance Show a => Show (Result a) where
    show (Success _ a)    = "Success " ++ show a
    show (Partial _)      = "Partial _"
    show (Failure _ errs) = "Failure: " ++ show errs

type ParseStep r = V.Bytes -> Result r

-- | Simple CPSed parser
--
newtype Parser a = Parser { runParser :: forall r .  (a -> ParseStep r) -> ParseStep r }

instance Functor Parser where
    fmap f (Parser pa) = Parser (\ k -> pa (\ a -> k (f a)))
    {-# INLINE fmap #-}
    a <$ Parser pb = Parser (\ k -> pb (\ _ -> k a))
    {-# INLINE (<$) #-}

instance Applicative Parser where
    pure x = Parser (\ k -> k x)
    {-# INLINE pure #-}
    Parser pf <*> Parser pa = Parser (\ k -> pf (\ f  -> pa (k . f)))
    {-# INLINE (<*>) #-}

instance Monad Parser where
    return = pure
    {-# INLINE return #-}
    Parser pa >>= f = Parser (\ k -> pa (\ a -> runParser (f a) k))
    {-# INLINE (>>=) #-}
    fail str = Parser (\ _ input -> Failure input str)
    {-# INLINE fail #-}

instance Fail.MonadFail Parser where
    fail str = Parser (\ _ input -> Failure input str)
    {-# INLINE fail #-}

instance MonadPlus Parser where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance Alternative Parser where
    empty = Parser (\ _ input -> Failure input "Std.Data.Parser.Base(Alternative).empty")
    {-# INLINE empty #-}
    f <|> g = do
        (r, bs) <- runAndKeepTrack f
        case r of
            Success input x -> Parser (\ k _ -> k x input)
            Failure _ _     -> pushBack bs >> g
            _               -> error "Std.Data.Parser.Base: impossible"
    {-# INLINE (<|>) #-}


-- | Parse the complete input, without resupplying
parse :: Parser a -> V.Bytes -> Either String a
{-# INLINE parse #-}
parse (Parser p) input = snd $ finishParsing (p (flip Success) input)

-- | Parse the complete input, without resupplying, return the rest bytes
parse' :: Parser a -> V.Bytes -> (V.Bytes, Either String a)
{-# INLINE parse' #-}
parse' (Parser p) input = finishParsing (p (flip Success) input)

-- | Parse an input chunk
parseChunk :: Parser a -> V.Bytes -> Result a
{-# INLINE parseChunk #-}
parseChunk (Parser p) = p (flip Success)

-- | Finish parsing and fetch result, feed empty bytes if it's 'Partial' result.
finishParsing :: Result a -> (V.Bytes, Either String a)
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
parseChunks :: Monad m => m V.Bytes -> Parser a -> V.Bytes -> m (V.Bytes, Either String a)
{-# INLINABLE parseChunks #-}
parseChunks m (Parser p) input = go m (p (flip Success) input)
  where
    go m r = case r of
        Partial f -> do
            inp <- m
            if V.null inp
            then go (return V.empty) (f V.empty)
            else go m (f inp)
        Success rest a    -> return (rest, Right a)
        Failure rest errs -> return (rest, Left errs)

-- | Run a parser and keep track of all the input it consumes.
-- Once it's finished, return the final result (always 'Success' or 'Failure') and
-- all consumed chunks.
--
runAndKeepTrack :: Parser a -> Parser (Result a, [V.Bytes])
{-# INLINE runAndKeepTrack #-}
runAndKeepTrack (Parser pa) = Parser $ \ k0 input ->
    let r0 = pa (\ a input' -> Success input' a) input in go [] r0 k0
  where
    go !acc r k0 = case r of
        Partial k        -> Partial (\ input -> go (input:acc) (k input) k0)
        Success input' _ -> k0 (r, reverse acc) input'
        Failure input' _ -> k0 (r, reverse acc) input'

pushBack :: [V.Bytes] -> Parser ()
{-# INLINE pushBack #-}
pushBack [] = return ()
pushBack bs = Parser (\ k input -> k () (V.concat (input : bs)))

-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'Partial'.
ensureN :: Int -> Parser ()
{-# INLINE ensureN #-}
ensureN n0 = Parser $ \ ks input -> do
    let l = V.length input
    if l >= n0
    then ks () input
    else Partial (go n0 ks [input] l)
  where
    go n0 ks acc l = \ !input' -> do
        let l' = V.length input'
        if l' == 0
        then Failure
            (V.concat (reverse (input':acc)))
            "Std.Data.Parser.Base.ensureN: Not enough bytes"
        else do
            let l'' = l + l'
            if l'' < n0
            then Partial (go n0 ks (input':acc) l'')
            else do
                let input'' = V.concat (reverse (input':acc))
                ks () input''

-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes.
endOfInput :: Parser Bool
{-# INLINE endOfInput #-}
endOfInput = Parser $ \ k inp ->
    if V.null inp
    then Partial (\ inp' -> k (V.null inp') inp')
    else k False inp

decodePrim :: forall a. UnalignedAccess a => Parser a
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

decodePrimLE :: forall a. UnalignedAccess (LE a) => Parser a
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

decodePrimBE :: forall a. UnalignedAccess (BE a) => Parser a
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
scan :: s -> (s -> Word8 -> Maybe s) -> Parser V.Bytes
{-# INLINE scan #-}
scan s0 f = scanChunks s0 f'
  where
    f' st (V.Vec arr s l) = go f st arr s s (s+l)
    go f st arr off !i !end
        | i < end = do
            let !w = A.indexPrimArray arr i
            case f st w of
                Just st' -> go f st' arr off (i+1) end
                _        ->
                    let !len1 = i - off
                        !len2 = end - off
                    in Right (V.Vec arr off len1, V.Vec arr i len2)
        | otherwise = Left st

-- | Similar to 'scan', but working on 'V.Bytes' chunks, The predicate
-- consumes a 'V.Bytes' chunk and transforms a state argument,
-- and each transformed state is passed to successive invocations of
-- the predicate on each chunk of the input until one chunk got splited to
-- @Right (V.Bytes, V.Bytes)@ or the input ends.
--
scanChunks :: s -> (s -> V.Bytes -> Either s (V.Bytes, V.Bytes)) -> Parser V.Bytes
{-# INLINE scanChunks #-}
scanChunks s0 consume = Parser (go s0 [])
  where
    go s acc k inp =
        case consume s inp of
            Left s' -> do
                let acc' = inp : acc
                Partial (go s' acc' k)
            Right (want,rest) ->
                k (V.concat (reverse (want:acc))) rest

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
peek :: Parser Word8
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
satisfy :: (Word8 -> Bool) -> Parser Word8
{-# INLINE satisfy #-}
satisfy p = do
    ensureN 1
    Parser (\ k inp ->
        let w = V.unsafeHead inp
        in if p w
            then k w (V.unsafeTail inp)
            else Failure inp "Std.Data.Parser.Base.satisfy")

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
--
satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Parser a
{-# INLINE satisfyWith #-}
satisfyWith f p = do
    ensureN 1
    Parser (\ k inp ->
        let w = f (V.unsafeHead inp)
        in if p w
            then k w (V.unsafeTail inp)
            else Failure inp "Std.Data.Parser.Base.satisfyWith")

-- | Match a specific byte.
--
word8 :: Word8 -> Parser ()
{-# INLINE word8 #-}
word8 w' = do
    ensureN 1
    Parser (\ k inp ->
        let w = V.unsafeHead inp
        in if w == w'
            then k () (V.unsafeTail inp)
            else Failure inp "Std.Data.Parser.Base.word8")

-- | Match any byte.
--
anyWord8 :: Parser Word8
{-# INLINE anyWord8 #-}
anyWord8 = decodePrim

-- | Match either a single newline byte @\'\\n\'@, or a carriage
-- return followed by a newline byte @\"\\r\\n\"@.
endOfLine :: Parser ()
{-# INLINE endOfLine #-}
endOfLine = do
    w <- decodePrim :: Parser Word8
    case w of
        10 -> return ()
        13 -> word8 10
        _  -> fail "endOfLine"

--------------------------------------------------------------------------------

-- | 'skip' N bytes.
--
skip :: Int -> Parser ()
{-# INLINE skip #-}
skip n
    | n <= 0 = return ()        -- we use unsafe slice, guard negative n here
    | otherwise =
        Parser (\ k inp ->
            let l = V.length inp
            in if l >= n
                then k () (V.unsafeDrop n inp)
                else Partial (go k (n-l)))
  where
    go k !n inp =
        let l = V.length inp
        in if l >= n
            then k () (V.unsafeDrop n inp)
            else if l == 0
                then Failure inp "Std.Data.Parser.Base.skip"
                else Partial (go k (n-l))

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
    go k p inp =
        let rest = V.dropWhile p inp    -- If we ever enter 'Partial', empty input
        in k () rest                    -- means 'endOfInput'

-- | Skip over white space using 'isSpace'.
--
skipSpaces :: Parser ()
{-# INLINE skipSpaces #-}
skipSpaces = skipWhile isSpace

take :: Int -> Parser V.Bytes
{-# INLINE take #-}
take n
    | n <= 0 = return V.empty   -- we use unsafe slice, guard negative n here
    | otherwise =
        Parser (\ k inp ->
            let l = V.length inp
            in if l >= n
                then k (V.unsafeTake n inp) (V.unsafeDrop n inp)
                else Partial (go k (n-l) [inp]))
  where
    go k !n acc inp =
        let l = V.length inp
        in if l >= n
            then
                let !r = V.concat (reverse (V.unsafeTake n inp:acc))
                in k r (V.unsafeDrop n inp)
            else if l == 0
                then Failure inp "Std.Data.Parser.Base.take: Not enough bytes"
                else Partial (go k (n-l) (inp:acc))

-- | Consume input as long as the predicate returns 'False' or reach the end of input,
-- and return the consumed input.
--
takeTill :: (Word8 -> Bool) -> Parser V.Bytes
{-# INLINE takeTill #-}
takeTill p = Parser (\ k inp ->
    let (want, rest) = V.break p inp
    in if V.null rest
        then Partial (go k [want])
        else k want rest)
  where
    go k acc inp =
        if V.null inp
        then k (V.concat (reverse acc)) inp
        else
            let (want, rest) = V.break p inp
                acc' = want : acc
            in if V.null rest
                then Partial (go k acc')
                else k (V.concat (reverse acc')) rest

-- | Consume input as long as the predicate returns 'True' or reach the end of input,
-- and return the consumed input.
--
takeWhile :: (Word8 -> Bool) -> Parser V.Bytes
{-# INLINE takeWhile #-}
takeWhile p = Parser (\ k inp ->
    let (want, rest) = V.span p inp
    in if V.null rest
        then Partial (go k [want])
        else k want rest)
  where
    go k acc inp =
        if V.null inp
        then k (V.concat (reverse acc)) inp
        else
            let (want, rest) = V.span p inp
                acc' = want : acc
            in if V.null rest
                then Partial (go k acc')
                else k (V.concat (reverse acc')) rest

-- | Similar to 'takeWhile', but requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or reach the end of input
--
takeWhile1 :: (Word8 -> Bool) -> Parser V.Bytes
{-# INLINE takeWhile1 #-}
takeWhile1 p = do
    bs <- takeWhile p
    if V.null bs then fail "Std.Data.Parser.Base.takeWhile1" else return bs


-- | @bytes s@ parses a sequence of bytes that identically match @s@.
--
bytes :: V.Bytes -> Parser ()
{-# INLINE bytes #-}
bytes bs = do
    let n = V.length bs
    Parser (\ k inp ->
        let l = V.length inp
        in if l >= n
            then
                if bs == (V.unsafeTake n inp)
                    then k () (V.unsafeDrop n inp)
                    else Failure inp "Std.Data.Parser.Base.bytes"
            else
                if inp == (V.unsafeTake l bs)
                    then Partial (go k (n-l) (V.unsafeDrop l bs))
                    else Failure inp "Std.Data.Parser.Base.bytes")
  where
    go k !n !bs inp =
        let l = V.length inp
        in if l >= n
            then
                if bs == (V.unsafeTake n inp)
                    then k () (V.unsafeDrop n inp)
                    else Failure inp "Std.Data.Parser.Base.bytes"
            else if l == 0
                then Failure inp "Std.Data.Parser.Base.bytes: Not enough bytes"
                else
                    if inp == (V.unsafeTake l bs)
                        then Partial (go k (n-l) (V.unsafeDrop l bs))
                        else Failure inp "Std.Data.Parser.Base.bytes"

-- | Same as 'bytes' but ignoring case.
bytesCI :: V.Bytes -> Parser ()
{-# INLINE bytesCI #-}
bytesCI bs = do
    let n = V.length bs'
    Parser (\ k inp ->
        let l = V.length inp
        in if l >= n
            then
                if bs' == CI.foldCase (V.unsafeTake n inp)
                    then k () (V.unsafeDrop n inp)
                    else Failure inp "Std.Data.Parser.Base.bytesCI"
            else
                if CI.foldCase inp == (V.unsafeTake l bs')
                    then Partial (go k (n-l) (V.unsafeDrop l bs'))
                    else Failure inp "Std.Data.Parser.Base.bytesCI")
  where
    bs' = CI.foldCase bs
    go k !n !bs inp =
        let l = V.length inp
        in if l >= n
            then
                if bs == CI.foldCase (V.unsafeTake n inp)
                    then k () (V.unsafeDrop n inp)
                    else Failure inp "Std.Data.Parser.Base.bytesCI"
            else if l == 0
                then Failure inp "Std.Data.Parser.Base.bytesCI: Not enough bytes"
                else
                    if CI.foldCase inp == (V.unsafeTake l bs)
                        then Partial (go k (n-l) (V.unsafeDrop l bs))
                        else Failure inp "Std.Data.Parser.Base.bytesCI"

-- | @text s@ parses a sequence of UTF8 bytes that identically match @s@.
--
text :: T.Text -> Parser ()
{-# INLINE text #-}
text (T.Text bs) = bytes bs
