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
data Result s a
    = Success !V.Bytes a
    | Failure !V.Bytes [String]
    | NeedMore (V.Bytes -> State# s -> (# State# s, Result s a #))

instance Functor (Result s) where
    fmap f (Success s a)   = Success s (f a)
    fmap _ (Failure s msg) = Failure s msg
    fmap f (NeedMore k) = NeedMore (\ bs s0# -> case k bs s0# of (# s2#, r #) -> (# s2#, fmap f r #))

instance Show a => Show (Result s a) where
    show (Failure _ errs) = "ParseFailure: " ++ show errs
    show (NeedMore _)     = "NeedMore _"
    show (Success _ a)    = "ParseOK " ++ show a

success :: r -> ParseStep s r
success x ba# offset# l# s0# = (# s0# , Success (V.Bytes ba# offset# l#) x #)

failure :: [String] -> a -> ParseStep s r
failure err _ ba# offset# l# s0# = (# s0#, Failure (V.Bytes ba# offset# l#) err #)

-- | @ParseStep@ is a function that extract value from a immutable (thus can be shared)
-- buffer.
--
-- We use unboxed 'State#' token here to support building arrays inside ST monad
-- without touching 'unsafePerformIO'.
--
type ParseStep s r = ByteArray# -> Int# -> Int# -> State# s -> (# State# s, Result s r #)

-- | @Parser@ is a monad to help compose @ParseStep@. With next @ParseStep@ continuation,
-- We can provide early termination.
--
newtype Parser a = Parser { runParser :: forall s r. (a -> ParseStep s r) -> ParseStep s r }

instance Functor Parser where
    fmap f (Parser p) = Parser (\ k -> p (\ x -> k (f x)))
    {-# INLINE fmap #-}
    a <$ (Parser p) = Parser (\ k -> p (\ _ -> k a))
    {-# INLINE (<$) #-}

instance Applicative Parser where
    pure x = Parser (\ k -> k x)
    {-# INLINE pure #-}
    (Parser pa) <*> (Parser pb) = Parser (\ k -> pa (\ bc -> pb (\ b -> k (bc b))))
    {-# INLINE (<*>) #-}

instance Monad Parser where
  return = pure
  {-# INLINE return #-}
  (Parser p) >>= f = Parser (\ k -> p (\ a -> runParser (f a) k))
  {-# INLINE (>>=) #-}
  (Parser p) >> (Parser q) = Parser (\ k -> p (\ _ ->  q k))
  {-# INLINE (>>) #-}
  fail str = Parser (failure [str])
  {-# INLINE fail #-}

instance Fail.MonadFail Parser where
  fail str = Parser (failure [str])
  {-# INLINE fail #-}
{-
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

parseST :: V.Bytes -> Parser a -> ST s (Result s a)
parseST (V.Bytes ba# offset# l#) (Parser p) = ST (p success ba# offset# l#)

parseIO :: V.Bytes -> Parser a -> IO (Result RealWorld a)
parseIO (V.Bytes ba# offset# l#) (Parser p) = IO (p success ba# offset# l#)

-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'NeedMore'.
ensureN :: Int# -> Parser ()
{-# INLINE ensureN #-}
ensureN n# = Parser $ \ k ba# offset# end# s0# ->
    let offset2# = offset# +# n#
    in case offset2# >=# end# of
        1# -> k () ba# offset2# end# s0#
        _  -> (# s0#, NeedMore (go n# k [V.Bytes ba# offset# end#] (end# -# offset#)) #)
  where
    go n# k acc len0# = \ input'@(V.Bytes _ _ len1#) s1# ->
        case len1# ==# 0# of
            1# ->
                let len2# = len0# +# len1#
                in case len2# >=# n# of
                    1# ->
                        let (V.Bytes ba3# offset3# len3#) = V.concat (reverse (input':acc))
                        in k () ba3# (offset3# +# n#) len3# s1#

                    _ -> (# s1#, NeedMore (go n# k (input':acc) len2#) #)
            _  -> (# s1#, Failure
                    (V.concat (reverse (input':acc)))
                    ["Std.Data.Parser.ensureN: Not enough bytes"] #)

decodePrim :: forall a. Int#
        -> (ByteArray# -> Int# -> a)
        -> Parser a
{-# INLINE decodePrim #-}
decodePrim n# read = ensureN n# >> Parser (\ k ba# offset# end# s0# ->
    k (read ba# offset#) ba# (offset# +# n#) end# s0#)
