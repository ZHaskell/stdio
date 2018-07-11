{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

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
import qualified Control.Monad.Fail as Fail
import Control.Monad
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
    | NeedMore (V.Bytes -> Result a)

instance Functor Result where
    fmap f (Success s a)   = Success s (f a)
    fmap _ (Failure s msg) = Failure s msg
    fmap f (NeedMore k) = NeedMore (fmap f . k)

instance Show a => Show (Result a) where
    show (Failure _ errs) = "ParseFailure: " ++ show errs
    show (NeedMore _)     = "NeedMore _"
    show (Success _ a)    = "ParseOK " ++ show a

-- | Simple parser structure
newtype Parser a = Parser
    { runParser :: forall r . V.Bytes         -- Current input
                -> (V.Bytes -> a -> Result r) -- The success continuation
                -> Result r                   -- We don't need failure continuation
    }                                         -- since the failure is written on the Result's tag


instance Monad Parser where
  return = pure
  {-# INLINE return #-}
  Parser pa >>= f = Parser (\ input k -> pa input (\ input' a -> runParser (f a) input' k))
  {-# INLINE (>>=) #-}
  fail str = Parser (\ input _ -> Failure input [str])
  {-# INLINE fail #-}

instance Fail.MonadFail Parser where
  fail str = Parser (\ input _ -> Failure input [str])
  {-# INLINE fail #-}

instance Applicative Parser where
    pure x = Parser (\ input k -> k input x)
    {-# INLINE pure #-}
    pf <*> pa = do { f <- pf; a <- pa; return (f a) }
    {-# INLINE (<*>) #-}

instance Functor Parser where
    fmap f (Parser pa) = Parser (\ input k -> pa input (\ input' a -> k input' (f a)))
    {-# INLINE fmap #-}
    a <$ Parser pb = Parser (\ input k -> pb input (\ input' _ -> k input' a))
    {-# INLINE (<$) #-}

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

instance Alternative Parser where
    empty = Parser (\ input _ -> Failure input ["Std.Data.Parser(Alternative).empty"])
    {-# INLINE empty #-}
    f <|> g = do
        (r, bs) <- runAndKeepTrack f
        case r of
            Success input x -> Parser (\ _ k -> k input x)
            Failure _ _ -> pushBack bs >> g
            _ -> error "Binary: impossible"
    {-# INLINE (<|>) #-}

-- | Run a parser and keep track of all the input it consumes.
-- Once it's finished, return the final result (always 'Success' or 'Failure') and
-- all consumed chunks.
--
runAndKeepTrack :: Parser a -> Parser (Result a, [V.Bytes])
{-# INLINE runAndKeepTrack #-}
runAndKeepTrack (Parser pa) = Parser $ \ input k0 ->
    let r0 = pa input (\ input' a -> Success input' a) in go [] r0 k0
  where
    go !acc r k0 = case r of
        NeedMore k -> NeedMore (\ input -> go (input:acc) (k input) k0)
        Success input' _    -> k0 input' (r, reverse acc)
        Failure input' _    -> k0 input' (r, reverse acc)

pushBack :: [V.Bytes] -> Parser ()
{-# INLINE pushBack #-}
pushBack [] = Parser (\ input k -> k input ())
pushBack bs = Parser (\ input k -> k (V.concat (input : bs)) ())

parse :: V.Bytes -> Parser a -> Result a
parse input (Parser p) = p input (\ input' a -> Success input' a)

-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'NeedMore'.
ensureN :: Int -> Parser ()
ensureN !n0 = Parser $ \ input ks -> do
    let l = V.length input
    if l >= n0
    then ks input ()
    else NeedMore (go ks [] l)
  where
    go ks acc l = \ input' -> do
        let l' = V.length input'
        if l' == 0
        then Failure
            (V.concat (reverse (input':acc)))
            ["Std.Data.Parser.ensureN: Not enough bytes"]
        else do
            let l'' = l + l'
            if l'' < n0
            then NeedMore (go ks (input':acc) l'')
            else do
                let input'' = V.concat (reverse (input':acc))
                ks input'' ()
