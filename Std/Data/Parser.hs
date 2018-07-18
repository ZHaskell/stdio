{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Fail                 as Fail
import           Data.Primitive.PrimArray
import           Data.Word
import           GHC.Prim
import           GHC.Types
import           Std.Data.PrimArray.UnalignedAccess
import qualified Std.Data.Vector                    as V

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
            Failure _ _ -> pushBack bs >> g
            _ -> error "Binary: impossible"
    {-# INLINE (<|>) #-}

parse :: Parser a -> V.Bytes -> Result a
{-# INLINABLE parse #-}
parse (Parser p) input = p (\ a input' -> Success input' a) input

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
        NeedMore k -> NeedMore (\ input -> go (input:acc) (k input) k0)
        Success input' _    -> k0 (r, reverse acc) input'
        Failure input' _    -> k0 (r, reverse acc) input'

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

decodePrim :: forall a. UnalignedAccess a => Parser a
{-# INLINE decodePrim #-}
decodePrim = do
    ensureN n
    Parser (\ k (V.PrimVector (PrimArray ba#) i@(I# i#) len) ->
        let !r = indexWord8ArrayAs ba# i#
        in k r (V.PrimVector (PrimArray ba#) (i + n) len))
  where
    n = (getUnalignedSize (unalignedSize :: UnalignedSize a))

decodePrimLE :: forall a. UnalignedAccess (LE a) => Parser a
{-# INLINE decodePrimLE #-}
decodePrimLE = getLE <$> decodePrim

decodePrimBE :: forall a. UnalignedAccess (BE a) => Parser a
{-# INLINE decodePrimBE #-}
decodePrimBE = getBE <$> decodePrim
