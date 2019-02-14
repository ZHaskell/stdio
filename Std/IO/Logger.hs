{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : Std.IO.Logger
Description : High performance logger
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

Simple, high performance logger. The design choice of this logger is biased towards simplicity instead of generlization:

    * All log functions lives in 'IO' , customization via 'changeDefaultLogger' at the start of the program.
    * The log format is basically fixed with simple JSON support.
    * Each logging thread are responsible for building log 'Builder's into a small 'V.Bytes' with line buffer
      instead of leaving all 'Builder's to the flushing thread:
        * We won't keep garbage for too long simply because they're referenced by log's 'Builder'.
        * Each logging thread only need perform a CAS to prepend log 'V.Bytes' into a list, which reduces contention.
        * Logging order is preserved under concurrent settings.

Flushing is automatic and throttled for 'debug', 'info', 'warn' to boost performance, while a 'fatal' log always flush logger's buffer, This also lead to a problem that if main thread exits too early logs may missed, to add a flushing when program exits, use 'withLogger' like:

@@@
import Std.IO.Logger

main :: IO ()
main = withLogger $ do
    ....
@@@
-}

module Std.IO.Logger
  ( -- * A simple Logger type
    Logger
  , LoggerConfig(..)
  , newLogger
  , changeDefaultLogger
  , withLogger
    -- * logging functions
  , debug
  , info
  , warn
  , fatal
  , otherLevel
  ) where

import Control.Monad
import Std.Data.Vector.Base as V
import Std.IO.LowResTimer
import Std.IO.StdStream
import Std.IO.Buffered
import System.IO.Unsafe (unsafePerformIO)
import GHC.Stack
import Data.Word8 (_space, _lf, _bracketleft, _bracketright)
import Data.IORef
import Control.Concurrent.MVar
import qualified Std.Data.Builder.Base as B
import qualified Std.Data.Builder.Numeric as B
import qualified Data.Time as Time
import Std.IO.Exception

data Logger = Logger
    { loggerFlush           :: IO ()
    , loggerThrottledFlush  :: IO ()
    , loggerBytesList       :: {-# UNPACK #-} !(IORef [V.Bytes])
    , loggerConfig          :: {-# UNPACK #-} !LoggerConfig
    }

-- | Logger configuration.
data LoggerConfig = LoggerConfig
    { loggerBufferSize       :: {-# UNPACK #-} !Int -- ^ Buffer size used when creating logger's 'BufferedOutput'
    , loggerMinFlushInterval :: {-# UNPACK #-} !Int -- ^ Minimal flush interval, see Notes on 'debug'
    , loggerTsCache          :: IO (B.Builder ())   -- ^ A IO action return a formatted date/time string
    , loggerLineBufSize      :: {-# UNPACK #-} !Int -- ^ Buffer size to build each log/line
    , loggerShowDebug        :: Bool                -- ^ Set to 'False' to filter debug logs
    , loggerShowTS           :: Bool                -- ^ Set to 'False' to disable auto data/time string prepending
    }

-- | A default logger config with
--
--   * debug ON
--   * data/time@%Y-%m-%dT%H:%M:%S%Z@ ON
--   * 0.1s minimal flush interval
--   * line buffer size 128 bytes
--   * 'BufferedOutput' buffer size equals to 'V.defaultChunkSize'.
defaultLoggerConfig :: LoggerConfig
{-# NOINLINE defaultLoggerConfig #-}
defaultLoggerConfig = unsafePerformIO $ do
    tsCache <- throttle 1 $ do
        t <- Time.getCurrentTime
        return . B.string8 $
            Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" t
    return $ LoggerConfig V.defaultChunkSize 10 tsCache 128 True True

flushLog :: Output o => MVar (BufferedOutput o) -> IORef [V.Bytes] -> IO ()
flushLog oLock bList =
    withMVar oLock $ \ o -> do
        bss <- atomicModifyIORef' bList (\ bss -> ([], bss))
        forM_ (reverse bss) (writeBuffer o)
        flushBuffer o

-- | Make a new logger
newLogger :: Output o
          => LoggerConfig
          -> o
          -> IO Logger
newLogger config o = do
    bList <- newIORef []
    oLock <- newMVar =<< newBufferedOutput o (loggerBufferSize config)
    let flush = flushLog oLock bList
    throttledFlush <- throttleTrailing_ (loggerMinFlushInterval config) flush
    return $ Logger flush throttledFlush bList config

defaultLogger :: IORef Logger
{-# NOINLINE defaultLogger #-}
defaultLogger = unsafePerformIO $
    newIORef =<< newLogger defaultLoggerConfig stderr

-- | Change default logger.
changeDefaultLogger :: Logger -> IO ()
changeDefaultLogger !logger = atomicWriteIORef defaultLogger logger

getDefaultLogger :: IO Logger
getDefaultLogger = readIORef defaultLogger

-- | Manually flush default logger.
flushDefaultLogger :: IO ()
flushDefaultLogger = getDefaultLogger >>= loggerFlush

pushLog :: IORef [V.Bytes] -> Int -> B.Builder () -> IO ()
pushLog blist bfsiz b = do
    let !bs = B.buildBytesWith bfsiz b
    atomicModifyIORef' blist (\ bss -> (bs:bss, ()))

-- | Flush default logger when program exits.
withLogger :: IO () -> IO ()
withLogger = (`finally` flushDefaultLogger)

--------------------------------------------------------------------------------

debug :: B.Builder () -> IO ()
debug = otherLevel "DEBUG" False

info :: B.Builder () -> IO ()
info = otherLevel "INFO" False

warn :: B.Builder () -> IO ()
warn = otherLevel "WARN" False

fatal :: B.Builder () -> IO ()
fatal = otherLevel "FATAL" True

otherLevel :: B.Builder ()      -- ^ log level
           -> Bool              -- ^ flush immediately?
           -> B.Builder ()      -- ^ log content
           -> IO ()
otherLevel level flushNow b = getDefaultLogger >>=
    \ (Logger flush throttledFlush blist (LoggerConfig _ _ tscache lbsiz showdebug showts)) -> do
        ts <- if showts then tscache else return ""
        when showdebug $ do
            pushLog blist lbsiz $ do
                B.encodePrim _bracketleft
                level
                B.encodePrim _bracketright
                B.encodePrim _space
                when showts $ ts >> B.encodePrim _space
                b
                B.encodePrim _lf
            if flushNow then flush else throttledFlush

