{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Std.Data.Parser.Base
Description : Efficient deserialization/parse.
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable
WIP
-}

module Std.Data.Parser.Base where

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Fail                 as Fail
import           Data.Primitive.PrimArray
import           Data.Word
import           GHC.Prim
import           GHC.Types
import           Prelude                            hiding (takeWhile)
import           Std.Data.PrimArray.UnalignedAccess
import qualified Std.Data.Vector.Base               as V
import qualified Std.Data.Vector.Extra              as V

-- | Simple parsing result, that represent respectively:
--
-- * failure: with the error message
--
-- * continuation: that need for more input data, supply empty bytes to indicate EOF
--
-- * success: the remaining unparsed data and the parser value
--
data Result a
    = Success !V.Bytes a
    | Failure !V.Bytes [String]
    | NeedMore (V.Bytes -> Result a)

instance Functor Result where
    fmap f (Success s a)   = Success s (f a)
    fmap _ (Failure s msg) = Failure s msg
    fmap f (NeedMore k)    = NeedMore (fmap f . k)

instance Show a => Show (Result a) where
    show (Failure _ errs) = "ParseFailure: " ++ show errs
    show (NeedMore _)     = "NeedMore _"
    show (Success _ a)    = "ParseOK " ++ show a

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
    fail str = Parser (\ _ input -> Failure input [str])
    {-# INLINE fail #-}

instance Fail.MonadFail Parser where
    fail str = Parser (\ _ input -> Failure input [str])
    {-# INLINE fail #-}

instance MonadPlus Parser where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance Alternative Parser where
    empty = Parser (\ _ input -> Failure input ["Std.Data.Parser(Alternative).empty"])
    {-# INLINE empty #-}
    f <|> g = do
        (r, bs) <- runAndKeepTrack f
        case r of
            Success input x -> Parser (\ k _ -> k x input)
            Failure _ _     -> pushBack bs >> g
            _               -> error "Std.Data.Parser.Base: impossible"
    {-# INLINE (<|>) #-}

-- | Parse an input chunk
parse :: Parser a -> V.Bytes -> Result a
{-# INLINABLE parse #-}
parse (Parser p) input = p (\ a input' -> Success input' a) input

-- | Parse the complete input, without resupplying
parseOnly :: Parser a -> V.Bytes -> Either [String] a
{-# INLINABLE parseOnly #-}
parseOnly (Parser p) input = go (p (\ a input' -> Success input' a) input)
  where
    go r = case r of
        Success _ a    -> Right a
        Failure _ errs -> Left errs
        NeedMore f     -> go (f V.empty)

-- | Run a parser with an initial input string, and a monadic action
-- that can supply more input if needed.
--
-- Note, once the monadic action return empty bytes, we will stop drawing
-- more bytes.
parseWith :: Monad m => m V.Bytes -> Parser a -> V.Bytes -> m (Result a)
{-# INLINABLE parseWith #-}
parseWith m (Parser p) input = go m (p (\ a input' -> Success input' a) input)
  where
    go m r = case r of
        NeedMore f -> do
            inp <- m
            if V.null inp
            then go (return V.empty) (f V.empty)
            else go m (f inp)
        r          -> return r

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
        NeedMore k       -> NeedMore (\ input -> go (input:acc) (k input) k0)
        Success input' _ -> k0 (r, reverse acc) input'
        Failure input' _ -> k0 (r, reverse acc) input'

pushBack :: [V.Bytes] -> Parser ()
{-# INLINE pushBack #-}
pushBack [] = return ()
pushBack bs = Parser (\ k input -> k () (V.concat (input : bs)))

-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'NeedMore'.
ensureN :: Int -> Parser ()
{-# INLINE ensureN #-}
ensureN n0 = Parser $ \ ks input -> do
    let l = V.length input
    if l >= n0
    then ks () input
    else NeedMore (go n0 ks [input] l)
  where
    go n0 ks acc l = \ !input' -> do
        let l' = V.length input'
        if l' == 0
        then Failure
            (V.concat (reverse (input':acc)))
            ["Std.Data.Parser.ensureN: Not enough bytes"]
        else do
            let l'' = l + l'
            if l'' < n0
            then NeedMore (go n0 ks (input':acc) l'')
            else do
                let input'' = V.concat (reverse (input':acc))
                ks () input''

-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes.
endOfInput :: Parser Bool
{-# INLINE endOfInput #-}
endOfInput = Parser $ \ k inp ->
    if V.null inp
    then NeedMore (\ inp' -> k (V.null inp') inp')
    else k False inp

decodePrim :: forall a. UnalignedAccess a => Parser a
{-# INLINE decodePrim #-}
decodePrim = do
    ensureN n
    Parser (\ k (V.PrimVector (PrimArray ba#) i@(I# i#) len) ->
        let !r = indexWord8ArrayAs ba# i#
        in k r (V.PrimVector (PrimArray ba#) (i+n) (len-n)))
  where
    n = (getUnalignedSize (unalignedSize :: UnalignedSize a))

decodePrimLE :: forall a. UnalignedAccess (LE a) => Parser a
{-# INLINE decodePrimLE #-}
decodePrimLE = getLE <$> decodePrim

decodePrimBE :: forall a. UnalignedAccess (BE a) => Parser a
{-# INLINE decodePrimBE #-}
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
            let !w = indexPrimArray arr i
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
                NeedMore (go s' acc' k)
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
satisfy p = do
    ensureN 1
    Parser (\ k inp ->
        let w = V.unsafeHead inp
        in if p w
            then k w (V.unsafeTail inp)
            else Failure inp ["satisfy"])
{-# INLINE satisfy #-}

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
--
satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Parser a
satisfyWith f p = do
    ensureN 1
    Parser (\ k inp ->
        let w = f (V.unsafeHead inp)
        in if p w
            then k w (V.unsafeTail inp)
            else Failure inp ["satisfyWith"])
{-# INLINE satisfyWith #-}

-- | Match a specific byte.
--
word8 :: Word8 -> Parser ()
word8 w' = do
    ensureN 1
    Parser (\ k inp ->
        let w = V.unsafeHead inp
        in if w == w'
            then k () (V.unsafeTail inp)
            else Failure inp ["word8"])
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
    Parser (\ k inp ->
        let w = V.unsafeHead inp
        in if p w
            then k () (V.unsafeTail inp)
            else Failure inp ["skipWord8"])
{-# INLINE skipWord8 #-}

--------------------------------------------------------------------------------

-- | 'skip' N bytes.
--
skip :: Int -> Parser ()
{-# INLINE skip #-}
skip n =
    Parser (\ k inp ->
        let l = V.length inp
        in if l >= n
            then k () (V.unsafeDrop n inp)
            else NeedMore (go k (n-l)))
  where
    go k n inp =
        let l = V.length inp
        in if l >= n
            then k () (V.unsafeDrop n inp)
            else if l == 0
                then Failure inp ["skip"]
                else NeedMore (go k (n-l))

-- | Skip past input for as long as the predicate returns 'True'.
--
skipWhile :: (Word8 -> Bool) -> Parser ()
{-# INLINE skipWhile #-}
skipWhile p =
    Parser (\ k inp ->
        let rest = V.dropWhile p inp
        in if V.null rest
            then NeedMore (go k p)
            else k () rest)
  where
    go k p inp =
        let rest = V.dropWhile p inp
        in if V.null rest
            then NeedMore (go k p)
            else k () rest


-- | Consume input as long as the predicate returns 'False' or reach the end of input,
-- and return the consumed input.
--
takeTill :: (Word8 -> Bool) -> Parser V.Bytes
{-# INLINE takeTill #-}
takeTill p = Parser (\ k inp ->
    let (want, rest) = V.break p inp
    in if V.null rest
        then NeedMore (go k [want])
        else k want rest)
  where
    go k acc inp =
        if V.null inp
        then k (V.concat (reverse acc)) inp
        else
            let (want, rest) = V.break p inp
                acc' = want : acc
            in if V.null rest
                then NeedMore (go k acc')
                else k (V.concat (reverse acc')) rest

-- | Consume input as long as the predicate returns 'True' or reach the end of input,
-- and return the consumed input.
--
takeWhile :: (Word8 -> Bool) -> Parser V.Bytes
{-# INLINE takeWhile #-}
takeWhile p = Parser (\ k inp ->
    let (want, rest) = V.span p inp
    in if V.null rest
        then NeedMore (go k [want])
        else k want rest)
  where
    go k acc inp =
        if V.null inp
        then k (V.concat (reverse acc)) inp
        else
            let (want, rest) = V.span p inp
                acc' = want : acc
            in if V.null rest
                then NeedMore (go k acc')
                else k (V.concat (reverse acc')) rest

-- | Similar to 'takeWhile', but requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or reach the end of input
--
takeWhile1 :: (Word8 -> Bool) -> Parser V.Bytes
{-# INLINE takeWhile1 #-}
takeWhile1 p = do
    bs <- takeWhile p
    if V.null bs then fail "takeWhile1" else return bs


-- | Skip over white space using 'isSpace'.
--
skipSpaces :: Parser ()
skipSpaces = skipWhile isSpace
{-# INLINE skipSpaces #-}

-- | @bytes s@ parses a sequence of bytes that identically match @s@.
--
bytes :: V.Bytes -> Parser ()
bytes bs = do
    let l = V.length bs
    ensureN l
    Parser (\ k inp ->
        if V.unsafeTake l inp == bs
            then k () (V.unsafeDrop l inp)
            else Failure inp ["bytes"])
{-# INLINE bytes #-}

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
