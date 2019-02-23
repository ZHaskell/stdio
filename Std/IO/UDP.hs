{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Std.IO.UDP
Description : UDP servers and clients
Copyright   : (c) Dong Han, 2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides an API for creating UDP sender and receiver.

-}

module Std.IO.UDP (
  -- * TCP Client
    UDP(..)
  , initUDP
  , UDPConfig(..)
  , defaultUDPConfig
  , UVUDPFlag(UV_UDP_DEFAULT, UV_UDP_IPV6ONLY, UV_UDP_REUSEADDR)
  , recvUDP
  , sendUDP
  ) where

import Data.Primitive.PrimArray as A
import           Data.Primitive.Ptr                 (copyPtrToMutablePrimArray)
import Data.IORef
import Std.Data.Array           as A
import Std.Data.Vector.Base     as V
import Std.IO.SockAddr
import Std.Foreign.PrimArray
import Std.IO.UV.FFI
import Std.IO.UV.Manager
import Std.IO.Exception
import Std.IO.Resource
import Data.Word
import Data.Int
import Data.Bits ((.&.))
import Control.Monad
import Control.Concurrent.MVar
import Foreign.Storable (peek, poke)
import Foreign.Ptr (plusPtr)

-- | UDP socket.
--
-- UDP socket is not thread safe, don't use it among multiple thread!
data UDP = UDP
    { udpHandle :: {-# UNPACK #-} !(Ptr UVHandle)
    , udpSlot    :: {-# UNPACK #-} !UVSlot
    , udpManager :: UVManager
    , udpRecvLargeBuffer ::  {-# UNPACK #-} !(A.MutablePrimArray RealWorld Word8)
    , udpRecvBufferSiz   :: {-# UNPACK #-} !Int32
    , udpRecvBufferArray ::  {-# UNPACK #-} !(A.MutablePrimArray RealWorld (Ptr Word8))
    , udpSendBuffer ::  {-# UNPACK #-} !(A.MutablePrimArray RealWorld Word8)
    , udpClosed  :: {-# UNPACK #-} !(IORef Bool)
    }

-- The buffer passing of UDP is a litte complicated here, to get maximum performance,
-- we do batch receiving. i.e. recv multiple messages inside libuv's event loop:
--
--   udpRecvLargeBuffer:
--
--   +---------+--------------+-----------+----------+--------+---------+------------
--   | buf siz | partial flag | addr flag |   addr   | buffer | buf siz | partial ...
--   +--4bytes-+----4bytes----+--4bytes---+-128bytes-+-bufsiz-+---------+------------
--   ^                                                        ^
--   |                                                        |
--   +---------------------+       +--------------------------+
--                         |       |
--                      +--+---+---+--+----
--   udpRecvBufferArray | buf0 | buf1 | ...
--                      +------+------+----
--
-- + we allocate a large buffer (buffer_size * buffer_number)
-- + each time we poke the udpRecvBufferArray and its last index (size - 1) to uv manager's buffer table.
-- + libuv side each alloc callback picks the last pointer from udpRecvBufferArray, decrease last index by 1
-- + the read result is write into the `buf siz` cell, then followed with partial flag, if addr is not NULL
--   then addr flag is 1 (otherwise 0), then addr if not NULL, the buffer is already written when recv callback
--   is called.
-- + On haskell side, we read buffer table's size, which is decreased by callback times. Then we poke those
--   received result out.

instance Show UDP where
    show (UDP handle slot uvm _ bufsiz _ _ _) =
        "UDP{udpHandle = " ++ show handle ++
                ",udpRecvBufferSiz = " ++ show bufsiz ++
                ",udpSlot = " ++ show slot ++
                ",udpManager =" ++ show uvm ++ "}"

-- | UDP options.
--
-- Though technically message length field in the UDP header is a max of 65535, but large packets
-- could be more likely dropped by routers, usually a packet with a payload <= 508 bytes is considered safe.
data UDPConfig = UDPConfig
    { recvMsgSize :: {-# UNPACK #-} !Int32      -- ^ maximum size of a received message
    , recvBatchSize :: {-# UNPACK #-} !Int      -- ^ how many messages we want to receive per uv loop,
                                                --   inside each uv_run, we do batch receiving,
                                                --   increase this number can improve receiving performance,
                                                --   at the cost of memory and potential GHC thread starving.
    , sendMsgSize :: {-# UNPACK #-} !Int        -- ^ maximum size of sending buffer
    , localUDPAddr   :: Maybe (SockAddr, UVUDPFlag) -- ^ do we want bind a local address before receiving & sending?
                                                    --   set to Nothing to let OS pick a random one.
    } deriving (Show, Eq, Ord)

-- | default 'UDPConfig', @defaultUDPConfig = UDPConfig 512 6 512 V.smallChunkSize Nothing@
defaultUDPConfig = UDPConfig 512 6 V.smallChunkSize Nothing

-- | Initialize a UDP socket, with fixed size receive buffer
--
initUDP :: HasCallStack
        => UDPConfig
        -> Resource UDP
initUDP (UDPConfig rbsiz rbArrSiz sbsiz maddr) = initResource
    (do uvm <- getUVManager
        -- (message size + sockaddr flag + + flag size) + sockaddr_in size + buffer
        -- see diagram above
        let rbufsiz'' =  140 + rbsiz'
        rbuf <- A.newPinnedPrimArray (fromIntegral rbufsiz'' * rbArrSiz')
        rbufArr <- A.newPinnedPrimArray rbArrSiz'

        -- initialize buffer array with right index
        withMutablePrimArrayContents rbuf $ \ p ->
            forM_ [0..rbArrSiz'-1] $ \ i -> do
                let bufNPtr = p `plusPtr` (i * fromIntegral rbufsiz'')
                writePrimArray rbufArr i bufNPtr

        (handle, slot) <- withUVManager uvm $ \ loop -> do
            handle <- hs_uv_handle_alloc loop
            slot <- getUVSlot uvm (peekUVHandleData handle)
            tryTakeMVar =<< getBlockMVar uvm slot  -- clear the parking spot

            -- init uv struct
            (do throwUVIfMinus_ (uv_udp_init loop handle)
                -- bind the socket if address is available
                forM_ maddr $ \ (addr, flag) ->
                    withSockAddr addr $ \ p ->
                        throwUVIfMinus_ (uv_udp_bind handle p flag)
                ) `onException` hs_uv_handle_free handle
            return (handle, slot)

        sbuf <- A.newPinnedPrimArray sbsiz'
        closed <- newIORef False
        return (UDP handle slot uvm rbuf rbsiz' rbufArr sbuf closed))
    closeUDP
  where
    rbsiz' = max 0 rbsiz
    rbArrSiz' = max 1 rbArrSiz
    sbsiz' = max 0 sbsiz

closeUDP :: UDP -> IO ()
closeUDP (UDP handle _ uvm _ _ _ _ closed) = withUVManager_ uvm $ do
    c <- readIORef closed
    unless c $ writeIORef closed True >> hs_uv_handle_close handle


-- | Recv messages from UDP socket, return source address if available, and a `Bool`
-- to indicate if the message is partial (larger than receive buffer size).
recvUDP :: HasCallStack => UDP -> IO [(Maybe SockAddr, Bool, V.Bytes)]
recvUDP (UDP handle slot uvm rbuf rbufsiz rbufArr _ closed) = mask_ $ do
    c <- readIORef closed
    if c
    then throwECLOSED
    else do
        m <- getBlockMVar uvm slot
        rbufArrSiz <- getSizeofMutablePrimArray rbufArr

        -- we have to reset the buffer size, during receiving it'll be overwritten
        forM_ [0..rbufArrSiz-1] $ \ i -> do
            p <- readPrimArray rbufArr i
            poke (castPtr p :: Ptr Int32) rbufsiz

        -- reset buffer table's size with buffer array's length, during receiving it'll be decreased
        withMutablePrimArrayContents rbufArr $ \ p ->
            pokeBufferTable uvm slot (castPtr p) rbufArrSiz

        withUVManager_ uvm $ do
            throwUVIfMinus_ (hs_uv_udp_recv_start handle)
            tryTakeMVar m

        r <- catch (takeMVar m) (\ (e :: SomeException) -> do
                withUVManager_ uvm (uv_udp_recv_stop handle)
                -- after we locked uvm and stop reading, the reading probably finished
                -- so try again
                r <- tryTakeMVar m
                case r of Just r -> return r
                          _      -> throwIO e)
        if r < rbufArrSiz
        then forM [rbufArrSiz-1, rbufArrSiz-2 .. r] $ \ i -> do
            p        <- readPrimArray rbufArr i
            -- see the buffer struct diagram above
            result   <- throwUVIfMinus (fromIntegral <$> peek @Int32 (castPtr p))
            flag     <- peek @Int32 (castPtr (p `plusPtr` 4))
            addrFlag <- peek @Int32 (castPtr (p `plusPtr` 8))
            !addr <- if addrFlag == 1
                then Just <$> peekSockAddr (castPtr (p `plusPtr` 12))
                else return Nothing
            let !partial = flag .&. UV_UDP_PARTIAL /= 0
            mba <- A.newPrimArray result
            copyPtrToMutablePrimArray mba 0 (p `plusPtr` 140) result
            ba <- A.unsafeFreezePrimArray mba
            return (addr, partial, V.PrimVector ba 0 result)
        else return []


-- | Send a UDP message to target address.
--
-- WARNING: message will be trimmed if its size is larger than 'sendMsgSize'.
sendUDP :: HasCallStack => UDP -> SockAddr -> V.Bytes  -> IO ()
sendUDP (UDP handle slot uvm _ _ _ sbuf closed) addr (V.PrimVector ba s la) = mask_ $ do
    c <- readIORef closed
    when c throwECLOSED
    -- copy message to pinned buffer
    lb <- getSizeofMutablePrimArray sbuf
    let l = min la lb
    copyPrimArray sbuf 0 ba s la
    withSockAddr addr $ \ paddr ->
        withMutablePrimArrayContents sbuf $ \ pbuf -> do
            (slot, m) <- withUVManager_ uvm $ do
                slot <- getUVSlot uvm (hs_uv_udp_send handle paddr pbuf l)
                m <- getBlockMVar uvm slot
                tryTakeMVar m
                return (slot, m)
            -- we can't cancel uv_udp_send_t in current libuv
            -- and disaster will happen if buffer got collected.
            -- so we have to turn to uninterruptibleMask_'s help.
            -- i.e. sendUDP is an uninterruptible operation.
            -- OS will guarantee writing a socket will not
            -- hang forever anyway.
            throwUVIfMinus_  (uninterruptibleMask_ $ takeMVar m)
