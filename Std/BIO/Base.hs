{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

module Std.BIO.Base where

import Data.Typeable
import Control.Exception

-- | The BIOError type is the root of the BIO exception type.
--
-- When implement BIO processors, all errors should be under BIOError's subhierarchy.
-- You can define your custom error with
--
-- @
--   data FooProcessError = FooProcessError deriving Show
--
--   instance Exception FooProcessError where
--     toException   = bioErrorToException
--     fromException = bioErrorFromException
-- @
data BIOError = forall e . Exception e => BIOError e

instance Show BIOError where show (BIOError e) = show e
instance Exception BIOError

bioErrorToException :: Exception e => e -> SomeException
bioErrorToException = toException . BIOError

bioErrorFromException :: Exception e => SomeException -> Maybe e
bioErrorFromException x = do
    BIOError a <- fromException x
    cast a

data BIOStep i o
    = Yield i o
    | Error i BIOError
    | Done i


type BIO m i o = (i, m i) -> m (BIOStep i o)



next :: Monad m => BIO m a b -> BIO m b c -> BIO m a c

