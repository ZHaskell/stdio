
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
Module      : Std.Data.Parser
Description : Efficient deserialization/parse.
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

WIP

-}
module Std.Data.Parser where

import qualified Std.Data.Vector as V
import Data.Primitive.ByteArray
import qualified Control.Monad.Fail as Fail
import Control.Monad
import GHC.Prim
import GHC.Types
import GHC.ST
import Control.Applicative

-- | Simple parsing result, that represent respectively:
--
-- * failure: with the error message
--
-- * continuation: that need for more input data
--
-- * success: the remaining unparsed data and the parser value
--
data Result a
    = Success !V.Bytes a
    | Failure !V.Bytes [String]

data ConsumeStrategy s
    = ConsumeStrict
    | ConsumeState (State# s -> (# State# s, V.Bytes #))

instance Functor Result where
    fmap f (Success s a)   = Success s (f a)
    fmap _ (Failure s msg) = Failure s msg

instance Show a => Show (Result a) where
    show (Failure _ errs) = "ParseFailure: " ++ show errs
    show (Success _ a)    = "ParseOK " ++ show a


-- | @ParseStep@ is a function that extract value from a immutable (thus can be shared)
-- buffer.
--
-- We use unboxed 'State#' token here to support building arrays inside ST monad
-- without touching 'unsafePerformIO'.
--
type ParseStep s r = ByteArray# -> Int# -> Int# -> State# s -> (# State# s, Result r #)

-- | @Parser@ is a monad to help compose @ParseStep@. With next @ParseStep@ continuation,
-- We can provide early termination.
--
newtype Parser a = Parser { runParser :: forall s r. ConsumeStrategy s -> (a -> ParseStep s r) -> ParseStep s r }

instance Functor Parser where
    fmap f (Parser p) = Parser (\ cs k -> p cs (k . f))
    {-# INLINE fmap #-}
    a <$ (Parser p) = Parser (\ cs k -> p cs (\ _ -> k a))
    {-# INLINE (<$) #-}

instance Applicative Parser where
    pure x = Parser (\ _ k -> k x)
    {-# INLINE pure #-}
    (Parser pa) <*> (Parser pb) = Parser (\ cs k -> pa cs (\ bc -> pb cs (k . bc)))
    {-# INLINE (<*>) #-}

instance Monad Parser where
  return = pure
  {-# INLINE return #-}
  (Parser p) >>= f = Parser (\ cs k -> p cs (\ a -> runParser (f a) cs k))
  {-# INLINE (>>=) #-}
  (Parser p) >> (Parser q) = Parser (\ cs k -> p cs (\ _ ->  q cs k))
  {-# INLINE (>>) #-}
{-
  fail str = Parser (failure [str])
  {-# INLINE fail #-}

instance Fail.MonadFail Parser where
  fail str = Parser (failure [str])
  {-# INLINE fail #-}
success :: r -> ParseStep s r
success x ba# offset# l# s0# = (# s0# , Success (V.Bytes ba# offset# l#) x #)

failure :: [String] -> a -> ParseStep s r
failure err _ ba# offset# l# s0# = (# s0#, Failure (V.Bytes ba# offset# l#) err #)
instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

instance Alternative Parser where
    {-# INLINE empty #-}
    empty = Parser (\ _ ba# offset# end# s0# ->
        (# s0#, Failure (V.Bytes ba# offset# end#)
            ["Std.Data.Parser(Alternative).empty"] #))
    {-# INLINE (<|>) #-}
    f <|> g = do
        (r, bs) <- runAndKeepTrack f
        case r of
            Success (V.Bytes ba# offset# end#) x -> Parser (\ _ k -> k x input ba# offset# end#)
            Failure _ _ -> pushBack bs >> g
            _ -> error "Binary: impossible"

-- | Run a parser and keep track of all the input it consumes.
-- Once it's finished, return the final result (always 'Success' or 'Failure') and
-- all consumed chunks.
--
runAndKeepTrack :: Parser a -> Parser (Result s a, [V.Bytes])
{-# INLINE runAndKeepTrack #-}
runAndKeepTrack (Parser p) = Parser $ \ k0 x s0# ->
    let r0 = p success in go [] r0 k0
  where
    go !acc r k0 = case r of
        NeedMore k -> NeedMore (\ ba# offset# l# -> go (V.Byte ba# offset# l#:acc) (k input) k0)
        Success (V.Bytes ba# offset# l#) _    -> k0 (r, reverse acc) ba# offset# l#
        Failure (V.Bytes ba# offset# l#) _    -> k0 (r, reverse acc) ba# offset# l#

pushBack :: [V.Bytes] -> Parser ()
{-# INLINE pushBack #-}
pushBack [] = Parser (\ k -> k ())
pushBack bs = Parser (\ k ba# offset# l# -> k () (V.concat ((V.Bytes ba# offset# l#) : bs)))
-}


-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'NeedMore'.
decodePrim :: Int#
            -> (ByteArray# -> Int# -> a)
            -> Parser a
{-# INLINE decodePrim #-}
decodePrim n0# read = Parser $ \ cs k ba0# offset0# end0# s0# ->
    let offset0'# = offset0# +# n0#
        (# s1#, ba1#, offset1#, end1# #) =
            case offset0'# <=# end0# of
                1# -> (# s0#, ba0#, offset0#, end0# #)
                _  -> consume cs ba0# offset0# end0# offset0'# s0#
    in case offset1# <=# end1# of
        1# -> k (read ba1# offset1#) ba1# (offset1# +# n0#) end1# s1#
        _  -> (# s1#, Failure undefined [""] #)
  where
    {-# NOINLINE consume #-}
    consume :: ConsumeStrategy s -> ByteArray# -> Int# -> Int# -> Int# -> State# s
            -> (# State# s, ByteArray#, Int#, Int# #)
    consume ConsumeStrict ba0# offset0# end0# offset0'# s0# = (# s0#, ba0#, offset0'#, end0# #)
