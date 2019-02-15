{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}


{-|
Module      : Std.IO.StdStream
Description : TTY devices
Copyright   : (c) Dong Han, 2018~2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides stdin\/stderr\/stdout reading and writings. Usually you don't have to use 'stderr' or 'stderrBuf' directly, 'Std.IO.Logger' provides more logging utilities through @stderr@. While 'stdinBuf' and 'stdoutBuf' is useful when you write interactive programs, 'Std.IO.Buffered' module provide many reading and writing operations. Example:

@@@
import Std.IO.LowResTimer
import Std.IO.Buffered
import Std.IO.StdStream (stdinBuf, stdoutBuf)

main = do
    -- read by '\n'
    b1 <- readLine stdinBuf
    -- read whatever user input in 3s, otherwise get Nothing
    b2 <- timeoutLowRes 30 $ readBuffered stdinBuf
    ...
    writeBuffer stdoutBuf
    flushBuffer stdoutBuf

@@@

-}
module Std.IO.StdStream
  ( -- * Standard input & output streams
    StdStream
  , isStdStreamTTY
  , stdin, stdout, stderr
  , stdinBuf, stdoutBuf
    -- * utils
  , printStd
  , readLineStd
  , putStd
  , putLineStd
  ) where

import Std.Data.Builder as B
import Std.Data.Vector as V
import Std.IO.UV.FFI
import Std.IO.UV.Manager
import Control.Monad
import Control.Concurrent.STM.TVar
import Control.Concurrent.MVar
import Std.IO.Exception
import Std.IO.UV.Errno
import System.IO.Unsafe
import Std.IO.Resource
import Std.IO.Buffered
import Std.Data.Vector.Base (defaultChunkSize)
import Foreign.Ptr

-- | Standard input and output streams
--
-- We support both regular file and TTY based streams, when initialized
-- 'uv_guess_handle' is called to decide which type of devices are connected
-- to standard streams.
--
-- 'StdStream' is different from other 'UVStream' in that exception during reading & writing
-- won't close 'StdStream'.

data StdStream
    = StdTTY {-# UNPACK #-}!(Ptr UVHandle) {-# UNPACK #-}!UVSlot UVManager -- similar to UVStream
    | StdFile {-# UNPACK #-}!UVFD                                          -- similar to UVFile

isStdStreamTTY :: StdStream -> Bool
isStdStreamTTY (StdTTY _ _ _) = True
isStdStreamTTY _              = False

instance Input StdStream where
    {-# INLINE readInput #-}
    readInput uvs@(StdTTY handle slot uvm) buf len = mask_ $ do
        m <- getBlockMVar uvm slot
        withUVManager_ uvm $ do
            throwUVIfMinus_ (hs_uv_read_start handle)
            pokeBufferTable uvm slot buf len
            tryTakeMVar m
        r <- takeMVar m
        if  | r > 0  -> return r
            -- r == 0 should be impossible, since we guard this situation in c side
            | r == fromIntegral UV_EOF -> return 0
            | r < 0 ->  throwUVIfMinus (return r)
    readInput (StdFile fd) buf len =
        throwUVIfMinus $ hs_uv_fs_read fd buf len (-1)

instance Output StdStream where
    {-# INLINE writeOutput #-}
    writeOutput (StdTTY handle _ uvm) buf len = mask_ $ do
        (slot, m) <- withUVManager_ uvm $ do
            slot <- getUVSlot uvm (hs_uv_write handle buf len)
            m <- getBlockMVar uvm slot
            tryTakeMVar m
            return (slot, m)
        throwUVIfMinus_  (takeMVar m)
    writeOutput (StdFile fd) buf len = go buf len
      where
        go !buf !bufSiz = do
            written <- throwUVIfMinus
                (hs_uv_fs_write fd buf bufSiz (-1))
            when (written < bufSiz)
                (go (buf `plusPtr` written) (bufSiz-written))

stdin :: StdStream
{-# NOINLINE stdin #-}
stdin = unsafePerformIO (makeStdStream 0)

stdout :: StdStream
{-# NOINLINE stdout #-}
stdout = unsafePerformIO (makeStdStream 1)

stderr :: StdStream
{-# NOINLINE stderr #-}
stderr = unsafePerformIO (makeStdStream 2)

stdinBuf :: BufferedInput StdStream
{-# NOINLINE stdinBuf #-}
stdinBuf = unsafePerformIO (newBufferedInput stdin defaultChunkSize)

stdoutBuf :: BufferedOutput StdStream
{-# NOINLINE stdoutBuf #-}
stdoutBuf = unsafePerformIO (newBufferedOutput stdout defaultChunkSize)

makeStdStream :: UVFD -> IO StdStream
makeStdStream fd = do
    typ <- uv_guess_handle fd
    if typ == UV_TTY
    then do
        uvm <- getUVManager
        withUVManager uvm $ \ loop -> do
            handle <- hs_uv_handle_alloc loop
            slot <- getUVSlot uvm (peekUVHandleData handle)
            tryTakeMVar =<< getBlockMVar uvm slot   -- clear the parking spot
            throwUVIfMinus_ (uv_tty_init loop handle (fromIntegral fd))
                `onException` hs_uv_handle_free handle
            return (StdTTY handle slot uvm)
    else return (StdFile fd)

--------------------------------------------------------------------------------

-- | print a 'Show' to stdout
printStd :: Show a => a -> IO ()
printStd s = do
    writeBuffer stdoutBuf (B.buildBytes . B.stringUTF8 . show $ s)
    flushBuffer stdoutBuf

-- | print a 'Builder' to stdout
putStd :: Builder a -> IO ()
putStd b = do
    writeBuffer stdoutBuf (B.buildBytes b)
    flushBuffer stdoutBuf

-- | print a 'Builder' to stdout, with a linefeed
putLineStd :: Builder a -> IO ()
putLineStd b = do
    writeBuffer stdoutBuf (B.buildBytes $ b >> B.char8 '\n')
    flushBuffer stdoutBuf

-- | read a line from stdin
readLineStd :: IO V.Bytes
readLineStd = readLine stdinBuf
