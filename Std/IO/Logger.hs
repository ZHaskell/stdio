{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : Std.IO.Logger
Description : High performance logger
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

High performance logger, WIP

-}

module Std.IO.Log where

newtype Logger = forall o. Output o => Logger
    { loggerOutput    :: MVar (BufferedOutput o)
    , loggerPrefix    :: Bytes -> IO Builder
    , loggerNoDebug   :: Bool
    , loggerAutoFlush :: Bool
    }

newLogger :: Output o => o -> Int -> Bool -> IO Logger


changeDefaultLogger :: Logger -> IO ()

getDefaultLogger :: IO Logger

--------------------------------------------------------------------------------

defaultLogger :: IORef Logger

debug :: Text -> IO ()

info :: Text -> IO ()

warn :: Text -> IO ()

fatal :: Text -> IO ()

debug_ :: Logger -> Text -> IO ()

info_ :: Logger -> Text -> IO ()

warn_ :: Logger -> Text -> IO ()

fatal_ :: Logger -> Text -> IO ()

