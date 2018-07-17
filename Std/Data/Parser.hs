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
    show (Failure rest errs) = "Failure: " ++ show errs ++ "\nStop at: " ++ show rest
    show (Success rest a)    = "Success: " ++ show a ++ "\nStop at: " ++ show rest

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
  fail str = Parser (failure [str])
  {-# INLINE fail #-}

instance Fail.MonadFail Parser where
  fail str = Parser (failure [str])
  {-# INLINE fail #-}

instance MonadPlus Parser where
    mzero = Parser (failure ["Std.Data.Parser(Alternative).mzero"])
    mplus = plus

instance Alternative Parser where
    {-# INLINE empty #-}
    empty = Parser (failure ["Std.Data.Parser(Alternative).empty"])
    {-# INLINE (<|>) #-}
    (<|>) = plus

plus :: Parser a -> Parser a -> Parser a
{-# INLINE plus #-}
plus (Parser p) (Parser q) = Parser (\ sc k ba0# offset0# end0# (s0# :: State# s) ->
    let (# getConsumed#, g# #) = case sc of
            ConsumeStrict ->
                (# (\ s0# -> (# s0#, ba0#, offset0#, end0# #))
                ,  (\ s0# -> (# s0#, V.empty #)) #)
            ConsumeState f# ->
                -- we use MutVar# to track consumed chunks in ConsumeState case
                let (# s1#, (var# :: MutVar# s [V.Bytes]) #) =
                        newMutVar# [V.Bytes ba0# offset0# (end0# -# offset0#)] s0#
                in (# (\ s0# ->
                        let (# s1#, consumed #) = readMutVar# var# s0#
                            (V.Bytes ba2# offset2# end2#) = V.concat (reverse consumed)
                        in (# s1#, ba2#, offset2#, end2# #))
                   ,  (\ s0# ->
                        let (# s1#, bs #)= f# s0#
                            (# s2#, consumed #) = readMutVar# var# s1#
                            s3# = writeMutVar# var# (bs:consumed) s2#
                        in (# s3#, bs #)) #)

    in case p (ConsumeState g#) k ba0# offset0# end0# s0# of
        succ@(# s1#, Success _ _ #) -> succ
        (# s1#, _ #) ->
            let (# s2#, ba2#, offset2#, end2# #) = getConsumed# s1#
            in q sc k ba2# offset2# end2# s2#)
{-
pushBack :: V.Bytes -> Parser ()
{-# INLINE pushBack #-}
pushBack (V.Bytes ba# offset# len#) = Parser (\ k ba# offset# l# ->

    k () (V.concat ((V.Bytes ba# offset# l#) : bs)))
-}

success :: r -> ParseStep s r
success x ba# offset# l# s0# = (# s0# , Success (V.Bytes ba# offset# (l# -# offset#)) x #)

failure :: [String] -> ConsumeStrategy s -> a -> ParseStep s r
failure err _ _ ba# offset# l# s0# = (# s0#, Failure (V.Bytes ba# offset# (l# -# offset#)) err #)

parse :: Parser a -> V.Bytes -> Result a
parse (Parser p) (V.Bytes ba# offset# len#) =
    runST (ST (p ConsumeStrict success ba# offset# (offset# +# len#)))

parseST :: Parser a -> ST s V.Bytes -> ST s (Result a)
parseST (Parser p) f@(ST f#) = do
    (V.Bytes ba# offset# len#) <- f
    ST (p (ConsumeState f#) success ba# offset# (offset# +# len#))

parseIO :: Parser a -> IO V.Bytes -> IO (Result a)
parseIO (Parser p) f@(IO f#) = do
    (V.Bytes ba# offset# len#) <- f
    IO (p (ConsumeState f#) success ba# offset# (offset# +# len#))


-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will draw new input chunk with 'ConsumeStrategy'.
--
decodePrim :: Int#
            -> (ByteArray# -> Int# -> a)
            -> Parser a
{-# INLINE decodePrim #-}
decodePrim n0# read = Parser $ \ cs k ba0# offset0# end0# s0# ->
    let (# s1#, ba1#, offset1#, end1#, offset1'# #) =
            let offset0'# = offset0# +# n0#
            in case offset0'# <=# end0# of
                1# -> (# s0#, ba0#, offset0#, end0#, offset0'# #)
                _  -> handleBoundary cs ba0# offset0# end0# n0# s0#
    -- We never pass partial appied k to boundary handling like other CPSed
    -- parsers, doing that will force ghc to think we're sharing k in both branch,
    -- thus kill inliner/unboxing, etc. Instead we directly handle boundary with
    -- ConsumeStrategy, and draw more bytes if necessary, thus k can be safely
    -- inlined, further unboxed by GHC.
    in case offset1'# of
        0# -> (# s1#, Failure (V.Bytes ba1# offset1# end1#)
                ["Std.Data.Parser.decodePrim: not enough bytes"] #)
        _  -> k (read ba1# offset1#) ba1# offset1'# end1# s1#

peekPrim :: Int#
          -> (ByteArray# -> Int# -> a)
          -> Parser a
{-# INLINE peekPrim #-}
peekPrim n0# read = Parser $ \ cs k ba0# offset0# end0# s0# ->
    let (# s1#, ba1#, offset1#, end1#, offset1'# #) =
            let offset0'# = offset0# +# n0#
            in case offset0'# <=# end0# of
                1# -> (# s0#, ba0#, offset0#, end0#, offset0'# #)
                _  -> handleBoundary cs ba0# offset0# end0# n0# s0#
    in case offset1'# of
        0# -> (# s1#, Failure (V.Bytes ba1# offset1# end1#)
                ["Std.Data.Parser.decodePrim: not enough bytes"] #)
        _  -> k (read ba1# offset1#) ba1# offset1# end1# s1#

handleBoundary :: ConsumeStrategy s -> ByteArray# -> Int# -> Int# -> Int# -> State# s
               -> (# State# s, ByteArray#, Int#, Int#, Int# #)
{-# NOINLINE handleBoundary #-}
handleBoundary ConsumeStrict ba0# offset0# end0# _ s0# = (# s0#, ba0#, offset0#, end0#, 0# #)
handleBoundary (ConsumeState f#) ba0# offset0# end0# n0# s0# =
    let len0# = end0# -# offset0#
        (# s1#, buf# #) = newByteArray# (n0# +# len0#) s0#
        s2# = copyByteArray# ba0# offset0# buf# 0# len0# s1#
    in consumeLoop f# buf# len0# (n0# +# len0#) s1#
  where
    consumeLoop :: (State# s -> (# State# s, V.Bytes #))
                -> MutableByteArray# s -> Int# -> Int# -> State# s
                -> (# State# s, ByteArray#, Int#, Int#, Int# #)
    consumeLoop f# buf0# len0# end# s0# =
            -- draw new chunk
            let (# s1#, (V.Bytes ba1# offset1# len1#) #) = f# s0#
            in case len1# /=# 0# of
                -- successfully draw a new chunk
                1# ->
                    let len2# = len0# +# len1#
                    in case len2# >=# end# of
                        -- we have got enough bytes
                        1# ->
                            let (# s2#, buf1# #) = resizeMutableByteArray# buf0# len2# s1#
                                s3# = copyByteArray# ba1# offset1# buf1# len0# len1# s2#
                                (# s4#, ba2# #) = unsafeFreezeByteArray# buf1# s3#
                            in (# s4#, ba2#, 0#, len2#, end# #)

                        -- we haven't got enough bytes, continue consume
                        _ ->
                            let s2# = copyByteArray# ba1# offset1# buf0# len0# len1# s1#
                            in consumeLoop f# buf0# len2# end# s2#

                -- the input is closed
                _  ->
                    let (# s2#, ba2# #) = unsafeFreezeByteArray# buf0# s1#
                    in (# s2#, ba2#, 0#, len0#, 0# #)
