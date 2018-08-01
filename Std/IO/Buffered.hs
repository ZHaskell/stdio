{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImplicitParams #-}

{-|
Module      : Std.IO.Buffered
Description : Buffered I/O interface
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide basic I/O interface stdio use.

-}

module Std.IO.Buffered where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.Primitive (ioToPrim, primToIO)
import           Control.Monad.ST
import           Data.IORef
import           Data.IORef.Unboxed
import           Data.Primitive.PrimArray
import           Data.Typeable
import           Data.Word
import           Foreign.C.Types          (CSize (..))
import           Foreign.Ptr
import           GHC.Prim
import           Std.Data.Array
import qualified Std.Data.Builder         as B
import qualified Std.Data.Text            as T
import qualified Std.Data.Text.UTF8Codec  as UTF8
import qualified Std.Data.Vector.Base     as V
import qualified Std.Data.Vector          as V
import           Std.Foreign.PrimArray
import           Std.IO.Exception
import           System.Posix.Types       (CSsize (..))

-- | Input device
--
-- Laws: 'readInput' should return 0 on EOF.
--
-- Note: 'readInput' is considered not thread-safe, e.g. A 'Input' device
-- can only be used with a single 'BufferedInput', If multiple 'BufferedInput' s
-- are opened on a same 'Input' device, the behaviour will be undefined.
--
class Input i where
    readInput :: HasCallStack => i -> Ptr Word8 -> Int -> IO Int

-- | Output device
--
-- Laws: 'writeOutput' should not return until all data are written (may not
-- necessarily flushed to hardware, that should be done in device specific way).

-- Note: 'writeOutput' is considered not thread-safe, e.g. A 'Output' device
-- can only be used with a single 'BufferedOutput', If multiple 'BufferedOutput' s
-- are opened on a same 'Input' device, the behaviour will be undefined.
--
class Output o where
    writeOutput :: HasCallStack => o -> Ptr Word8 -> Int -> IO ()

data BufferedInput i = BufferedInput
    { bufInput    :: i
    , bufPushBack :: {-# UNPACK #-} !(IORef V.Bytes)
    , inputBuffer :: {-# UNPACK #-} !(IORef (MutablePrimArray RealWorld Word8))
    }

data BufferedOutput o = BufferedOutput
    { bufOutput     :: o
    , bufIndex      :: {-# UNPACK #-} !Counter
    , outputBuffer  :: {-# UNPACK #-} !(MutablePrimArray RealWorld Word8)
    }

data BufferedInputConfig = BufferedInputConfig
    { inputBufSiz  :: Int
    , inputTimeout :: Int
    , inputMinRate :: Int
    }

data BufferedOutputConfig = BufferedOutputConfig
    { outputBufSiz     :: Int
    , outputTimeout    :: Int
    , outputAsyncFlush :: Bool
    }

newBufferedInput :: input
                 -> Int
                 -> IO (BufferedInput input)
newBufferedInput i bufSiz = do
    pb <- newIORef V.empty
    buf <- newPinnedPrimArray bufSiz
    inputBuffer <- newIORef buf
    return (BufferedInput i pb inputBuffer)

newBufferedOutput :: output
                  -> Int    -- ^ Output buffer size
                  -> IO (BufferedOutput output)
newBufferedOutput o bufSiz = do
    index <- newIORefU 0
    buf <- newPinnedPrimArray bufSiz
    return (BufferedOutput o index buf)

-- | Request bytes from 'BufferedInput'.
--
-- The buffering logic is quite simple:
--
-- If we have pushed back bytes, directly return it, otherwise we read using buffer size.
-- If we read N bytes, and N is larger than half of the buffer size, then we freeze buffer and return,
-- otherwise we copy buffer into result and reuse buffer afterward.
--
readBuffer :: (HasCallStack, Input i) => BufferedInput i -> IO V.Bytes
readBuffer BufferedInput{..} = do
    pb <- readIORef bufPushBack
    if V.null pb
    then do
        rbuf <- readIORef inputBuffer
        bufSiz <- getSizeofMutablePrimArray rbuf
        l <- readInput bufInput (mutablePrimArrayContents rbuf) bufSiz
        if l < bufSiz `quot` 2                -- read less than half size
        then do
            mba <- newPrimArray l              -- copy result into new array
            copyMutablePrimArray mba 0 rbuf 0 l
            ba <- unsafeFreezePrimArray mba
            return $! V.fromArr ba 0 l
        else do                                -- freeze buf into result
            when (bufSiz /= 0) $ do
                buf' <- newPinnedPrimArray bufSiz
                writeIORef inputBuffer buf'
            ba <- unsafeFreezePrimArray rbuf
            return $! V.fromArr ba 0 l
    else do
        writeIORef bufPushBack V.empty
        return pb

-- | Read exactly N bytes
--
-- If EOF reached before N bytes read, a 'ShortReadException' will be thrown
--
readExactly :: (HasCallStack, Input i) => Int -> BufferedInput i -> IO V.Bytes
readExactly n h = V.concat `fmap` (go h n)
  where
    go h n = do
        chunk <- readBuffer h
        let l = V.length chunk
        if l > n
        then do
            let (lastChunk, rest) = V.splitAt n chunk
            unReadBuffer rest h
            return [lastChunk]
        else if l == n
            then return [chunk]
            else if l == 0
                then
                    throwIO (ShortReadException
                        (IOEInfo "" "unexpected EOF reached" callStack))
                else do
                    chunks <- go h (n - l)
                    return (chunk : chunks)

data ShortReadException = ShortReadException IOEInfo deriving (Show, Typeable)

instance Exception ShortReadException where
    toException = ioExceptionToException
    fromException = ioExceptionFromException


-- | Push bytes back into buffer
--
unReadBuffer :: (HasCallStack, Input i) => V.Bytes -> BufferedInput i -> IO ()
unReadBuffer pb' BufferedInput{..} = do
    modifyIORef' bufPushBack $ \ pb -> pb' `V.append` pb

-- | Read until reach a magic bytes
--
-- If EOF reached before meet a magic byte, a 'ShortReadException' will be thrown.

readToMagic :: (HasCallStack, Input i) => Word8 -> BufferedInput i -> IO V.Bytes
readToMagic magic h = V.concat `fmap` (go h magic)
  where
    go h magic = do
        chunk <- readBuffer h
        if V.null chunk
        then throwIO (ShortReadException
            (IOEInfo "" "unexpected EOF reached" callStack))
        else case V.elemIndex magic chunk of
            Just i -> do
                let (lastChunk, rest) = V.splitAt (i+1) chunk
                unReadBuffer rest h
                return [lastChunk]
            Nothing -> do
                chunks <- go h magic
                return (chunk : chunks)

-- | Read a line
--
readLine :: (HasCallStack, Input i) => BufferedInput i -> IO T.Text
readLine h = do
    bss <- go h (V.c2w '\n')
    case T.validateUTF8 (V.concat bss) of
        T.Success t -> return t
        T.PartialBytes _ bs ->
            throwIO (UTF8PartialBytesException bs
                (IOEInfo "" "utf8 decode error" callStack))
        T.InvalidBytes bs ->
            throwIO (UTF8InvalidBytesException bs
                (IOEInfo "" "utf8 decode error" callStack))
  where
    go h magic = do
        chunk <- readBuffer h
        if V.null chunk
        then return []
        else case V.elemIndex magic chunk of
            Just i -> do
                let (lastChunk, rest) = V.splitAt (i+1) chunk
                unReadBuffer rest h
                return [lastChunk]
            Nothing -> do
                chunks <- go h magic
                return (chunk : chunks)


-- | Read a chunk of utf8 text
--
readTextChunkUTF8 :: (HasCallStack, Input i) => BufferedInput i -> IO T.Text
readTextChunkUTF8 h@BufferedInput{..} = do
    chunk <- readBuffer h
    if V.null chunk
    then return T.empty
    else do
        let (V.PrimVector vba vs vl) = chunk
            minLen = UTF8.decodeCharLen vba vs
        if minLen > vl                              -- is this chunk partial?
        then do                                     -- if so, try continue reading first
            rbuf <- readIORef inputBuffer
            bufSiz <- getSizeofMutablePrimArray rbuf
            copyPrimArray rbuf 0 vba vs vl         -- copy the partial chunk into buffer and try read new bytes
            l <- readInput bufInput (mutablePrimArrayContents rbuf `plusPtr` vl) (bufSiz - vl)
            let l' = l + vl
            if l' < bufSiz `quot` 2                -- read less than half size
            then if l' == vl
                then do                             -- no new bytes read, partial before EOF
                    throwIO (UTF8PartialBytesException chunk
                        (IOEInfo "" "utf8 decode error" callStack))
                else do
                    mba <- newPrimArray l'              -- copy result into new array
                    copyMutablePrimArray mba 0 rbuf 0 l'
                    ba <- unsafeFreezePrimArray mba
                    decode (V.fromArr ba 0 l')
            else do                                 -- freeze buf into result
                rbuf' <- newPinnedPrimArray bufSiz
                writeIORef inputBuffer rbuf'
                ba <- unsafeFreezePrimArray rbuf
                decode (V.fromArr ba 0 l')
        else decode chunk
  where
    decode bs = case T.validateUTF8 bs of
        T.Success t -> return t
        T.PartialBytes t bs -> do
            unReadBuffer bs h
            return t
        T.InvalidBytes bs -> throwIO (UTF8InvalidBytesException bs
                (IOEInfo "" "utf8 decode error" callStack))


data UTF8DecodeException
    = UTF8InvalidBytesException V.Bytes IOEInfo
    | UTF8PartialBytesException V.Bytes IOEInfo
  deriving (Show, Typeable)
instance Exception UTF8DecodeException where
    toException = ioExceptionToException
    fromException = ioExceptionFromException

--------------------------------------------------------------------------------

-- | Write 'V.Bytes' into buffered handle.
--
-- Copy 'V.Bytes' to buffer if it can hold, otherwise
-- write both buffer(if not empty) and 'V.Bytes'.
--
writeBuffer :: (Output o) => BufferedOutput o -> V.Bytes -> IO ()
writeBuffer o@BufferedOutput{..} v@(V.PrimVector ba s l) = do
    i <- readIORefU bufIndex
    bufSiz <- getSizeofMutablePrimArray outputBuffer
    if i + l <= bufSiz
    then do
        -- current buffer can hold it
        copyPrimArray outputBuffer i ba s l   -- copy to buffer
        writeIORefU bufIndex (i+l)              -- update index
    else do
        if (i > 0)
        then do
            -- flush the buffer
            withMutablePrimArrayContents outputBuffer $ \ ptr -> writeOutput bufOutput ptr i
            writeIORefU bufIndex 0

            writeBuffer o v -- try write to buffer again
        else
            withPrimVectorSafe v (writeOutput bufOutput)


-- | Write 'V.Bytes' into buffered handle.
--
-- Copy 'V.Bytes' to buffer if it can hold, otherwise
-- write both buffer(if not empty) and 'V.Bytes'.
--
writeBuilder :: (Output o) => BufferedOutput o -> B.Builder a -> IO ()
writeBuilder BufferedOutput{..} (B.Builder b) = do
    i <- readIORefU bufIndex
    originBufSiz <- getSizeofMutablePrimArray outputBuffer
    _ <- primToIO (b (B.OneShotAction action) (lastStep originBufSiz) (B.Buffer outputBuffer i))
    return ()
  where
    action :: V.Bytes -> ST RealWorld ()
    action bytes = ioToPrim (withPrimVectorSafe bytes (writeOutput bufOutput))

    lastStep :: Int -> a -> B.BuildStep RealWorld
    lastStep originBufSiz _ (B.Buffer buf offset)
        | sameMutablePrimArray buf outputBuffer = ioToPrim $ do
            writeIORefU bufIndex offset   -- record new buffer index
            return []
        | offset >= originBufSiz = ioToPrim $ do
            withMutablePrimArrayContents buf $ \ ptr -> writeOutput bufOutput ptr offset
            writeIORefU bufIndex 0
            return [] -- to match 'BuildStep' return type
        | otherwise = ioToPrim $ do
            copyMutablePrimArray outputBuffer 0 buf 0 offset
            writeIORefU bufIndex offset
            return [] -- to match 'BuildStep' return type

-- | Flush the buffer(if not empty).
--
flush :: Output f => BufferedOutput f -> IO ()
flush BufferedOutput{..} = do
    i <- readIORefU bufIndex
    withMutablePrimArrayContents outputBuffer $ \ ptr -> writeOutput bufOutput ptr i
    writeIORefU bufIndex 0
