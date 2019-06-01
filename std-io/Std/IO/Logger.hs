{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : Std.IO.Logger
Description : High performance logger
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Simple, high performance logger. The design choice of this logger is biased towards simplicity instead of generlization:

    * All log functions lives in 'IO'.
    * A logger connected to stderr, 'setStdLogger' are always available.
    * Each logging thread are responsible for building log 'Builder's into a small 'V.Bytes' with line buffer
      instead of leaving all 'Builder's to the flushing thread so that:
        * We won't keep garbage for too long simply because they're referenced by log's 'Builder'.
        * Each logging thread only need perform a CAS to prepend log 'V.Bytes' into a list, which reduces contention.
        * Each log is atomic, Logging order is preserved under concurrent settings.

Flushing is automatic and throttled for 'debug', 'info', 'warn' to boost performance, while a 'fatal' log always flush logger's buffer, This also lead to a problem that if main thread exits too early logs may missed, to add a flushing when program exits, use 'withLogger' like:

@
import Std.IO.Logger

main :: IO ()
main = withStdLogger $ do
    ....
@
-}

module Std.IO.Logger
  ( -- * A simple Logger type
    Logger
  , LoggerConfig(..)
  , newLogger
  , loggerFlush
  , setStdLogger
  , getStdLogger
  , withStdLogger
    -- * logging functions
  , debug
  , info
  , warn
  , fatal
  , otherLevel
    -- * logging functions with specific logger
  , debugWith
  , infoWith
  , warnWith
  , fatalWith
  , otherLevelWith
  ) where

import Control.Monad
import Std.Data.Vector.Base as V
import Std.IO.LowResTimer
import Std.IO.StdStream
import Std.IO.Buffered
import System.IO.Unsafe (unsafePerformIO)
import Std.IO.Exception
import Data.IORef
import Control.Concurrent.MVar
import qualified Std.Data.Builder.Base as B
import qualified Std.Data.Builder.Numeric as B
import qualified Data.Time as Time
import Std.IO.Exception

data Logger = Logger
    { loggerFlush           :: IO ()                -- ^ flush logger's buffer to output device
    , loggerThrottledFlush  :: IO ()
    , loggerBytesList       :: {-# UNPACK #-} !(IORef [V.Bytes])
    , loggerConfig          :: {-# UNPACK #-} !LoggerConfig
    }

-- | Logger configuration.
data LoggerConfig = LoggerConfig
    { loggerMinFlushInterval :: {-# UNPACK #-} !Int -- ^ Minimal flush interval, see Notes on 'debug'
    , loggerTsCache          :: IO (B.Builder ())   -- ^ A IO action return a formatted date/time string
    , loggerLineBufSize      :: {-# UNPACK #-} !Int -- ^ Buffer size to build each log/line
    , loggerShowDebug        :: Bool                -- ^ Set to 'False' to filter debug logs
    , loggerShowTS           :: Bool                -- ^ Set to 'False' to disable auto data/time string prepending
    }

-- | A default logger config with
--
--   * debug ON
--   * 0.1s minimal flush interval
--   * defaultTSCache
--   * line buffer size 128 bytes
--   * show debug True
--   * show timestamp True
--   * 'BufferedOutput' buffer size equals to 'V.defaultChunkSize'.
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig 1 defaultTSCache 128 True True

-- | A default timestamp cache with format @%Y-%m-%dT%H:%M:%S%Z@
defaultTSCache :: IO (B.Builder ())
{-# NOINLINE defaultTSCache #-}
defaultTSCache = unsafePerformIO $ do
    throttle 1 $ do
        t <- Time.getCurrentTime
        return . B.string8 $
            Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" t

flushLog :: (HasCallStack, Output o) => MVar (BufferedOutput o) -> IORef [V.Bytes] -> IO ()
flushLog oLock bList =
    withMVar oLock $ \ o -> do
        bss <- atomicModifyIORef' bList (\ bss -> ([], bss))
        forM_ (reverse bss) (writeBuffer o)
        flushBuffer o

-- | Make a new logger
newLogger :: Output o
          => LoggerConfig
          -> BufferedOutput o
          -> IO Logger
newLogger config o = do
    bList <- newIORef []
    oLock <- newMVar o
    let flush = flushLog oLock bList
    throttledFlush <- throttleTrailing_ (loggerMinFlushInterval config) flush
    return $ Logger flush throttledFlush bList config

globalLogger :: IORef Logger
{-# NOINLINE globalLogger #-}
globalLogger = unsafePerformIO $
    newIORef =<< newLogger defaultLoggerConfig stderrBuf

-- | Change stderr logger.
setStdLogger :: Logger -> IO ()
setStdLogger !logger = atomicWriteIORef globalLogger logger

getStdLogger :: IO Logger
getStdLogger = readIORef globalLogger

-- | Manually flush stderr logger.
flushDefaultLogger :: IO ()
flushDefaultLogger = getStdLogger >>= loggerFlush

pushLog :: IORef [V.Bytes] -> Int -> B.Builder () -> IO ()
pushLog blist bfsiz b = do
    let !bs = B.buildBytesWith bfsiz b
    atomicModifyIORef' blist (\ bss -> (bs:bss, ()))

-- | Flush stderr logger when program exits.
withStdLogger :: IO () -> IO ()
withStdLogger = (`finally` flushDefaultLogger)

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
otherLevel level flushNow b =
    getStdLogger >>= \ logger -> otherLevelWith logger level flushNow b

--------------------------------------------------------------------------------

debugWith :: Logger -> B.Builder () -> IO ()
debugWith logger = otherLevelWith logger "DEBUG" False

infoWith :: Logger -> B.Builder () -> IO ()
infoWith logger = otherLevelWith logger "INFO" False
warnWith :: Logger -> B.Builder () -> IO ()

warnWith logger = otherLevelWith logger "WARN" False
fatalWith :: Logger -> B.Builder () -> IO ()

fatalWith logger = otherLevelWith logger "FATAL" False

otherLevelWith :: Logger
               -> B.Builder ()      -- ^ log level
               -> Bool              -- ^ flush immediately?
               -> B.Builder ()      -- ^ log content
               -> IO ()
otherLevelWith logger level flushNow b = case logger of
    (Logger flush throttledFlush blist (LoggerConfig _ tscache lbsiz showdebug showts)) -> do
        ts <- if showts then tscache else return ""
        when showdebug $ do
            pushLog blist lbsiz $ do
                B.char8 '['
                level
                B.char8 ']'
                B.char8 ' '
                when showts $ ts >> B.char8 ' '
                b
                B.char8 '\n'
            if flushNow then flush else throttledFlush
