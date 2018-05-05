{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Data.Parser where

import qualified Data.Vector as V
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad
import Control.Applicative


-- | Simple parsing result, that represent respectively:
--
-- * failure: with the error message
--
-- * continuation: that need for more inp data
--
-- * success: the remaining unparsed data and the parser value
--
data Result a
    = Success !V.Bytes a
    | Failure !V.Bytes [String]
    | NeedMore (Maybe V.Bytes -> Result a)

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
    { runParser :: forall r . V.Bytes
                -> (V.Bytes -> a -> Result r) -- The success continuation
                -> Result r                     -- We don't need failure continuation
    }                                             -- since the failure is written on the ParseResult tag


instance Monad Parser where
  return = pure
  {-# INLINE return #-}
  Parser pa >>= f = Parser (\ inp k -> pa inp (\ inp' a -> runParser (f a) inp' k))
  {-# INLINE (>>=) #-}
#if MIN_VERSION_base(4,9,0)
  fail = Fail.fail
instance Fail.MonadFail Parser where
#endif
  fail str = Parser (\ inp _ -> Failure inp [str])
  {-# INLINE fail #-}

instance Applicative Parser where
    pure x = Parser (\ inp k -> k inp x)
    {-# INLINE pure #-}
    pf <*> pa = do { f <- pf; a <- pa; return (f a) }
    {-# INLINE (<*>) #-}

instance Functor Parser where
    fmap f (Parser pa) = Parser (\ inp k -> pa inp (\ inp' a -> k inp' (f a)))
    {-# INLINE fmap #-}
    a <$ Parser pb = Parser (\ inp k -> pb inp (\ inp' _ -> k inp' a))
    {-# INLINE (<$) #-}

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

instance Alternative Parser where
    empty = Parser (\ inp _ -> Failure inp ["Data.Parser(Alternative).empty"])
    {-# INLINE empty #-}
    f <|> g = do
        (r, bs) <- runAndKeepTrack f
        case r of
            Success inp x -> Parser (\ _ k -> k inp x)
            Failure _ _ -> pushBack bs >> g
            _ -> error "Binary: impossible"
    {-# INLINE (<|>) #-}

-- | Run a parser and keep track of all the inp it consumes.
-- Once it's finished, return the final result (always 'Success' or 'Failure') and
-- all consumed chunks.
--
runAndKeepTrack :: Parser a -> Parser (Result a, [V.Bytes])
runAndKeepTrack (Parser pa) = Parser $ \ inp k0 ->
    let r0 = pa inp (\ inp' a -> Success inp' a) in go [] r0 k0
  where
    go !acc r k0 = case r of
        NeedMore k -> NeedMore (\ minp -> go (maybe acc (:acc) minp) (k minp) k0)
        Success inp' _    -> k0 inp' (r, reverse acc)
        Failure inp' _    -> k0 inp' (r, reverse acc)
{-# INLINE runAndKeepTrack #-}

pushBack :: [V.Bytes] -> Parser ()
pushBack [] = Parser (\ inp k -> k inp ())
pushBack bs = Parser (\ inp k -> k (V.concat (inp : bs)) ())
{-# INLINE pushBack #-}

{-
parse :: V.Bytes -> Parser a -> Result a
parse input (Parser p) = p input (\ input' a -> Success input' a)

-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'Partial'.
ensureN :: Int -> Parser ()
ensureN !n0 = C $ \inp ks -> do
  if V.length inp >= n0
    then ks inp ()
    else runCont (withInputChunks n0 enoughChunks onSucc onFail >>= put) inp ks
  where -- might look a bit funny, but plays very well with GHC's inliner.
        -- GHC won't inline recursive functions, so we make ensureN non-recursive
    enoughChunks n str
      | V.length str >= n = Right (str,V.empty)
      | otherwise = Left (n - V.length str)
    -- Sometimes we will produce leftovers lists of the form [V.empty, nonempty]
    -- where `nonempty` is a non-empty ByteString. In this case we can avoid a copy
    -- by simply dropping the empty prefix. In principle ByteString might want
    -- to gain this optimization as well
    onSucc = V.concat . dropWhile V.null
    onFail bss = C $ \_ _ -> Failure (V.concat bss) "not enough bytes"
-}
