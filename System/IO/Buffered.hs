{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImplicitParams #-}

{-|
Module      : System.IO.Buffered
Description : Buffered I/O interface
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide basic I/O interface stdio use.

-}

module System.IO.Buffered where


import System.IO.Exception
import System.IO.Resource
import System.IO.UV.Internal
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.ST
import GHC.Prim
import Foreign.Ptr
import Foreign.C.Types (CSize(..))
import System.Posix.Types (CSsize(..))
import Foreign.PrimArray
import Data.Word
import Data.IORef
import Data.IORef.Unboxed
import Data.Typeable
import Data.Primitive.PrimArray
import Data.Array
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Builder as B
import qualified Data.Text.UTF8Codec as UTF8
import GHC.Stack.Compat

-- | Input device
--
-- Laws:
--
--    * 'readInput' should return 0 on EOF.
--    * A single 'Input' can only be used with a single 'BufferedInput',
--      This is done by providing an 'inputBuffered' return 'True' only once.
--      Follow-up 'newBufferedInput' will failed with 'ResourceBusy'.
--
-- The reason for not support concurrent read registration are:
--
--    * In libuv only regular file read can be read concurrently by providing
--      different @uv_fs_t@ and offset, but concurrent reads on regular files
--      are never performant either on spinning disk or SSDs due to readahead
--      optimization.
--
--    * For devices which have no concept of offset, conncurrent read does not
--      make sense since there's no way to make sure each read is start from
--      a message's boundary.
--
--
class Input i where
    inputBuffered :: i -> IO Bool
    readInput :: HasCallStack => i -> Ptr Word8 -> Int -> IO Int

-- | Output device
--
-- Laws:
--
--    * 'initBufferedOutput' return a 'Resource' which can be acquired multiple times,
--      this is done by providing a 'createWriteUVReq' which returns a new 'Ptr' 'UVReq'
--      that does not affect others.
--
--    * 'writeOutput' should not return until all data are written (may not necessarily
--      flushed to hardware, that should be done in device specific way).
--
-- The 'Output' interface support concurrent write registration because:
--
--    * Libuv supports many devices' concurrent write operations with 'uv_req_t'.
--
--    * Unlike read, queued write operations will not messed up each message's
--      boundary, thus still useful on device with no concept of offset.
--
-- Note: on device doesn't support cancelling write request (basically everything except
-- regular file), the 'Ptr' 'UVReq' will only get freed when the device itself
-- is closed, i.e. 'destoryWriteUVReq' is a no-op. Thus it's not OK to create many
-- 'UVReq' s or 'BufferedOutput' s (for example use this 'Resource' with a resource
-- 'Pool') in case of memory leaks.
--
class Output o where
    createWriteUVReq  :: o -> IO (Ptr UVReq)
    destroyWriteUVReq :: o -> Ptr UVReq -> IO ()
    writeOutput :: HasCallStack => o -> Ptr UVReq -> Ptr Word8 -> Int -> IO ()

data BufferedInput i = BufferedInput
    { bufInput    :: i
    , bufPushBack :: {-# UNPACK #-} !(IORef V.Bytes)
    , inputBuffer :: {-# UNPACK #-} !(IORef (MutablePrimArray RealWorld Word8))
    }

data BufferedOutput o =
    BufferedOutput
        { bufOutput     :: o
        , bufIndex      :: {-# UNPACK #-} !(IORefU Int)
        , outputBuffer  :: {-# UNPACK #-} !(MutablePrimArray RealWorld Word8)
        , writeUVReq     :: {-# UNPACK #-} !(Ptr UVReq)
        }
    | ZeroBufferedOutput
        { bufOutput     :: o
        , writeUVReq     :: {-# UNPACK #-} !(Ptr UVReq)
        }

-- | Create buffered input device from an 'Input'.
--
-- User are not allow to create 'BufferedInput' with 0 buffer size, an 'InvalidArgument'
-- will be thrown.
--
-- Created 'BufferedInput' is NOT thread safe, it should be protect by 'MVar'
-- if used across multiple haskell threads, which is rare since unparsed
-- input stream have no concept of boundary.
--
newBufferedInput :: HasCallStack => input -> Int -> IO (BufferedInput input)
newBufferedInput i bufSiz = do
    when (bufSiz <= 0)
        (throwIO $ InvalidArgument
            (IOEInfo "EWRONGBUFSIZE" "input buffer size should be > 0" callStack))
    pb <- newIORef V.empty
    buf <- newPinnedPrimArray bufSiz
    inputBuffer <- newIORef buf
    return (BufferedInput i pb inputBuffer)

-- | Turn a 'Output' into a 'BufferedOutput' 'Resource'.
--
-- The returned 'Resource' can be acquired multiple times to support concurrent
-- write registration.
--
-- Created 'BufferedOutput' is NOT thread safe, it should be protect by 'MVar'
-- if used across multiple haskell threads, If the writing buffer's atomicity is
-- not required, use multiple 'BufferedOutput' s is more performant.
--
-- When using with regular files, multiple 'BufferedOutput' s should be adjusted to
-- different starting offset, and writes should be kept on separate offset ranges.
-- Otherwise the writing result is undefined. On other types of device, messages
-- from different 'BufferedOutput' are sent in flush order.
--
-- It's recommended to use a large buffer size (>4K) since write a lot of small message
-- is slow, especially under concurrent write scenario. But a 'BufferedOutput' with
-- buffer size 0 can also be open, in which case only a write context is created,
-- and all write operations on that 'BufferedOutput' are not buffered actually.
--
initBufferedOutput :: Output output => output -> Int -> Resource (BufferedOutput output)
initBufferedOutput o bufSiz = initResource openBufferedOutput closeBufferedOutput
  where
    openBufferedOutput = do
        when (bufSiz < 0)
            (throwIO $ InvalidArgument
                (IOEInfo "EWRONGBUFSIZE" "output buffer size should be >= 0" callStack))
        if bufSiz == 0
        then do
            req <- createWriteUVReq o
            return (ZeroBufferedOutput o req)
        else do
            index <- newIORefU 0
            buf <- newPinnedPrimArray bufSiz
            req <- createWriteUVReq o
            return (BufferedOutput o index buf req)
    closeBufferedOutput bo = destroyWriteUVReq (bufOutput bo) (writeUVReq bo)

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
        if l == 0
        then return V.empty                         -- EOF
        else if l < bufSiz `quot` 2                 -- read less than half size
            then do
                mba <- newPrimArray l               -- copy result into new array
                copyMutablePrimArray mba 0 rbuf 0 l
                ba <- unsafeFreezePrimArray mba
                return $! V.fromArr ba 0 l
            else do                                 -- freeze buf into result
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
-- flush buffer first(if not empty), then try to write again.
--
writeBytes :: (Output o) => BufferedOutput o -> V.Bytes -> IO ()
writeBytes ZeroBufferedOutput{..} v = do
    withPrimVectorSafe v (writeOutput bufOutput writeUVReq)
writeBytes o@BufferedOutput{..} v@(V.PrimVector ba s l) = do
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
            withMutablePrimArrayContents outputBuffer $ \ ptr -> writeOutput bufOutput writeUVReq ptr i
            writeIORefU bufIndex 0
            -- try write to buffer again
            writeBytes o v
        else
            withPrimVectorSafe v (writeOutput bufOutput writeUVReq)

-- | Write 'Ptr' 'Word8' into buffered handle.
--
-- Copy 'Ptr' 'Word8' to buffer if it can hold, otherwise
-- flush buffer first(if not empty), then try to write again.
--
writePtr :: (Output o) => BufferedOutput o -> Ptr Word8 -> Int -> IO ()
writePtr ZeroBufferedOutput{..} ptr l = writeOutput bufOutput writeUVReq ptr l
writePtr o@BufferedOutput{..} ptr l = do
    i <- readIORefU bufIndex
    bufSiz <- getSizeofMutablePrimArray outputBuffer
    if i + l <= bufSiz
    then do
        -- current buffer can hold it
        copyMutablePrimArrayFromPtr outputBuffer i ptr l  -- copy to buffer
        writeIORefU bufIndex (i+l)                        -- update index
    else do
        if (i > 0)
        then do
            -- flush the buffer
            writeOutput bufOutput writeUVReq ptr i
            writeIORefU bufIndex 0
            -- try write to buffer again
            writePtr o ptr l
        else
            writeOutput bufOutput writeUVReq ptr l


-- | Write 'V.Bytes' into buffered handle.
--
-- Copy 'V.Bytes' to buffer if it can hold, otherwise
-- write both buffer(if not empty) and 'V.Bytes'.
--
writeBuilder :: (Output o) => BufferedOutput o -> B.Builder -> IO ()
writeBuilder ZeroBufferedOutput{..} b = B.buildAndRun
    (\ v -> withPrimVectorSafe v (writeOutput bufOutput writeUVReq)) b
writeBuilder BufferedOutput{..} (B.Builder b) = do
    i <- readIORefU bufIndex
    originBufSiz <- getSizeofMutablePrimArray outputBuffer
    _ <- b (B.OneShotAction action) (lastStep originBufSiz) (B.Buffer outputBuffer i)
    return ()
  where
    action bytes = withPrimVectorSafe bytes (writeOutput bufOutput writeUVReq)

    lastStep originBufSiz (B.Buffer buf offset)
        | sameMutablePrimArray buf outputBuffer = do
            writeIORefU bufIndex offset   -- record new buffer index
            return []
        | offset >= originBufSiz = do
            withMutablePrimArrayContents buf $ \ ptr -> writeOutput bufOutput writeUVReq ptr offset
            writeIORefU bufIndex 0
            return [] -- to match 'BuildStep' return type
        | otherwise = do
            copyMutablePrimArray outputBuffer 0 buf 0 offset
            writeIORefU bufIndex offset
            return [] -- to match 'BuildStep' return type

-- | Flush the buffer(if not empty).
--
flush :: Output f => BufferedOutput f -> IO ()
flush ZeroBufferedOutput{..} = return ()
flush BufferedOutput{..} = do
    i <- readIORefU bufIndex
    withMutablePrimArrayContents outputBuffer $ \ ptr -> writeOutput bufOutput writeUVReq ptr i
    writeIORefU bufIndex 0
