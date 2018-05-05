{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}

module System.IO.UV.Internal where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Data.Int
import System.Posix.Types (CSsize(..))
import System.IO.Exception
import Data.Bits
import System.IO.Net.SockAddr (SockAddr, SocketFamily(..))

#include "uv.h"
#include "hs_uv.h"

type UVSlot = Int 
type UVFD = Int32

aCCEPT_BUFFER_SIZE = #const ACCEPT_BUFFER_SIZE

--------------------------------------------------------------------------------

data UVLoopData

peekUVEventQueue :: Ptr UVLoopData -> IO (CSize, Ptr CSize)
peekUVEventQueue p = (,)
    <$> (#{peek hs_loop_data, event_counter          } p)
    <*> (#{peek hs_loop_data, event_queue            } p)

clearUVEventCounter :: Ptr UVLoopData -> IO ()
clearUVEventCounter p = do
    #{poke hs_loop_data, event_counter          } p $ (0 :: CSize)

peekUVBufferTable :: Ptr UVLoopData -> IO (Ptr (Ptr Word8), Ptr CSsize)
peekUVBufferTable p = (,)
    <$> (#{peek hs_loop_data, buffer_table          } p)
    <*> (#{peek hs_loop_data, buffer_size_table     } p)

--------------------------------------------------------------------------------

data UVLoop

newtype UVRunMode = UVRunMode CInt 
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVRunMode, UVRunMode, 
  uV_RUN_DEFAULT = UV_RUN_DEFAULT,
  uV_RUN_ONCE    = UV_RUN_ONCE,
  uV_RUN_NOWAIT  = UV_RUN_NOWAIT}

-- | Peek loop data pointer from uv loop  pointer.
peekUVLoopData :: Ptr UVLoop -> IO (Ptr UVLoopData)
peekUVLoopData p = #{peek uv_loop_t, data} p

-- | An uv loop initResource with given slot size.
initUVLoop :: HasCallStack => Int -> Resource (Ptr UVLoop)
initUVLoop siz = initResource 
    (throwOOMIfNull $ hs_uv_loop_init (fromIntegral siz :: CSize))
    hs_uv_loop_close
foreign import ccall unsafe hs_uv_loop_init      :: CSize -> IO (Ptr UVLoop)
foreign import ccall unsafe hs_uv_loop_close     :: Ptr UVLoop -> IO ()

-- | Resize the uv loop to given slot size, throwOOMIfNull if fail.
uvLoopResize :: HasCallStack => Ptr UVLoop -> Int -> IO (Ptr UVLoop)
uvLoopResize loop newsiz = throwOOMIfNull $ hs_uv_loop_resize loop (fromIntegral newsiz :: CSize)
foreign import ccall unsafe hs_uv_loop_resize    :: Ptr UVLoop -> CSize -> IO (Ptr UVLoop)

-- | uv_run with usafe FFI.
uvRun :: HasCallStack => Ptr UVLoop -> UVRunMode -> IO CInt
uvRun loop mode = throwUVIfMinus $ uv_run loop mode
foreign import ccall unsafe uv_run            :: Ptr UVLoop -> UVRunMode -> IO CInt

-- | uv_run with safe FFI.
uvRunSafe :: HasCallStack => Ptr UVLoop -> UVRunMode -> IO CInt
uvRunSafe loop mode = throwUVIfMinus $ uv_run_safe loop mode
foreign import ccall safe "uv_run" uv_run_safe :: Ptr UVLoop -> UVRunMode -> IO CInt

-- | uv_run with usafe FFI.
uvAlive :: HasCallStack => Ptr UVLoop -> IO CInt
uvAlive loop = throwUVIfMinus $ uv_loop_alive loop
foreign import ccall unsafe uv_loop_alive :: Ptr UVLoop -> IO CInt

--------------------------------------------------------------------------------

data UVHandle

pokeUVHandleData :: Ptr UVHandle -> UVSlot -> IO ()
pokeUVHandleData p slot =  #{poke uv_handle_t, data} p (fromIntegral slot :: CIntPtr)

uvFileno :: HasCallStack => Ptr UVHandle -> IO UVFD
uvFileno = throwUVIfMinus . hs_uv_fileno
foreign import ccall unsafe hs_uv_fileno :: Ptr UVHandle -> IO UVFD

foreign import ccall unsafe hs_uv_handle_alloc  :: UVHandleType -> IO (Ptr UVHandle)
foreign import ccall unsafe hs_uv_handle_free :: Ptr UVHandle -> IO ()
foreign import ccall unsafe hs_uv_handle_close :: Ptr UVHandle -> IO ()

newtype UVHandleType = UVHandleType CInt 
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVHandleType, UVHandleType,
    uV_UNKNOWN_HANDLE  = UV_UNKNOWN_HANDLE,
    uV_ASYNC           = UV_ASYNC,
    uV_CHECK           = UV_CHECK,
    uV_FS_EVENT        = UV_FS_EVENT,
    uV_FS_POLL         = UV_FS_POLL,
    uV_HANDLE          = UV_HANDLE,
    uV_IDLE            = UV_IDLE,
    uV_NAMED_PIPE      = UV_NAMED_PIPE,
    uV_POLL            = UV_POLL,
    uV_PREPARE         = UV_PREPARE,
    uV_PROCESS         = UV_PROCESS,
    uV_STREAM          = UV_STREAM,
    uV_TCP             = UV_TCP,
    uV_TIMER           = UV_TIMER,
    uV_TTY             = UV_TTY,
    uV_UDP             = UV_UDP,
    uV_SIGNAL          = UV_SIGNAL,
    uV_FILE            = UV_FILE,
    uV_HANDLE_TYPE_MAX = UV_HANDLE_TYPE_MAX }

--------------------------------------------------------------------------------
-- uv_req_t

data UVReq

pokeUVReqData :: Ptr UVReq -> UVSlot -> IO ()
pokeUVReqData p slot =  #{poke uv_req_t, data} p (fromIntegral slot :: CIntPtr)

initUVReq :: HasCallStack => UVReqType -> Resource (Ptr UVReq)
initUVReq typ = initResource (throwOOMIfNull (hs_uv_req_alloc typ)) hs_uv_req_free

foreign import ccall unsafe hs_uv_req_alloc :: UVReqType -> IO (Ptr UVReq)
foreign import ccall unsafe hs_uv_req_free :: Ptr UVReq -> IO ()

newtype UVReqType = UVReqType CInt
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVReqType, UVReqType,
    uV_UNKNOWN_REQ      = UV_UNKNOWN_REQ,
    uV_REQ              = UV_REQ,
    uV_CONNECT          = UV_CONNECT,
    uV_WRITE            = UV_WRITE,
    uV_SHUTDOWN         = UV_SHUTDOWN,
    uV_UDP_SEND         = UV_UDP_SEND,
    uV_FS               = UV_FS,
    uV_WORK             = UV_WORK,
    uV_GETADDRINFO      = UV_GETADDRINFO,
    uV_GETNAMEINFO      = UV_GETNAMEINFO,
    uV_REQ_TYPE_MAX     = UV_REQ_TYPE_MAX }

--------------------------------------------------------------------------------

initUVAsyncWake :: HasCallStack => Ptr UVLoop -> Resource (Ptr UVHandle)
initUVAsyncWake loop = initResource 
    (do handle <- throwOOMIfNull (hs_uv_handle_alloc uV_ASYNC)
        throwUVIfMinus_ (hs_uv_async_wake_init loop handle) `onException` (hs_uv_handle_free handle)
        return handle
    )
    (hs_uv_handle_close) -- handle is free in uv_close callback

foreign import ccall unsafe hs_uv_async_wake_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt

uvAsyncSend :: HasCallStack => Ptr UVHandle -> IO ()
uvAsyncSend = throwUVIfMinus_ . uv_async_send
foreign import ccall unsafe uv_async_send :: Ptr UVHandle -> IO CInt

--------------------------------------------------------------------------------

initUVTimer :: HasCallStack => Ptr UVLoop -> Resource (Ptr UVHandle)
initUVTimer loop = initResource 
    (do handle <- throwOOMIfNull (hs_uv_handle_alloc uV_TIMER)
        throwUVIfMinus_ (uv_timer_init loop handle) `onException` (hs_uv_handle_free handle)
        return handle
    )
    (hs_uv_handle_close) -- handle is free in uv_close callback
foreign import ccall unsafe uv_timer_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt

uvTimerWakeStart :: HasCallStack => Ptr UVHandle -> Word64 -> IO ()
uvTimerWakeStart handle timeo = throwUVIfMinus_ $ hs_uv_timer_wake_start handle timeo
foreign import ccall unsafe hs_uv_timer_wake_start :: Ptr UVHandle -> Word64 -> IO CInt

uvTimerStart :: HasCallStack => Ptr UVHandle -> Word64 -> Word64 -> IO ()
uvTimerStart handle timeo repeat = throwUVIfMinus_ $ uv_timer_start handle timeo repeat
foreign import ccall unsafe uv_timer_start :: Ptr UVHandle -> Word64 -> Word64 -> IO CInt

uvTimerAgain :: HasCallStack => Ptr UVHandle -> IO ()
uvTimerAgain = throwUVIfMinus_ . uv_timer_again
foreign import ccall unsafe uv_timer_again :: Ptr UVHandle -> IO CInt

uvTimerSetRepeat :: HasCallStack => Ptr UVHandle -> Word64 -> IO ()
uvTimerSetRepeat handle timeo = throwUVIfMinus_ $ uv_timer_set_repeat handle timeo
foreign import ccall unsafe  uv_timer_set_repeat :: Ptr UVHandle -> Word64 -> IO CInt 

uvTimerStop :: HasCallStack => Ptr UVHandle -> IO ()
uvTimerStop = throwUVIfMinus_ . uv_timer_stop
foreign import ccall unsafe uv_timer_stop :: Ptr UVHandle -> IO CInt

--------------------------------------------------------------------------------

uvTCPOpen :: HasCallStack => Ptr UVHandle -> UVFD -> IO ()
uvTCPOpen handle sock = throwUVIfMinus_ $ hs_uv_tcp_open handle sock
foreign import ccall unsafe hs_uv_tcp_open :: Ptr UVHandle -> UVFD -> IO CInt

uvTCPInit :: HasCallStack => Ptr UVLoop -> Ptr UVHandle -> IO ()
uvTCPInit loop handle = throwUVIfMinus_ $ uv_tcp_init loop handle
foreign import ccall unsafe uv_tcp_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt

foreign import ccall unsafe uv_tcp_init_ex :: Ptr UVLoop -> Ptr UVHandle -> CUInt -> IO CInt

uvTCPNodelay :: HasCallStack => Ptr UVHandle -> Bool -> IO ()
uvTCPNodelay handle nodelay = throwUVIfMinus_ $ uv_tcp_nodelay handle (if nodelay then 1 else 0)
foreign import ccall unsafe uv_tcp_nodelay :: Ptr UVHandle -> CInt -> IO CInt

uvTCPKeepAlive :: HasCallStack => Ptr UVHandle -> Maybe CUInt -> IO ()
uvTCPKeepAlive handle Nothing = throwUVIfMinus_ $ uv_tcp_keepalive handle 0 0
uvTCPKeepAlive handle (Just delay) = throwUVIfMinus_ $ uv_tcp_keepalive handle 1 delay
foreign import ccall unsafe uv_tcp_keepalive :: Ptr UVHandle -> CInt -> CUInt -> IO CInt

uvTCPBind :: HasCallStack => Ptr UVHandle -> Ptr SockAddr -> Bool -> IO ()
uvTCPBind handle addr ipv6_only = throwUVIfMinus_ $
    uv_tcp_bind handle addr (if ipv6_only then #{const UV_TCP_IPV6ONLY} else 0)
foreign import ccall unsafe uv_tcp_bind :: Ptr UVHandle -> Ptr SockAddr -> CUInt -> IO CInt

uvTCPConnect :: HasCallStack => Ptr UVReq -> Ptr UVHandle -> Ptr SockAddr -> IO ()
uvTCPConnect req handle addr = throwUVIfMinus_ $ hs_uv_tcp_connect req handle addr
foreign import ccall unsafe hs_uv_tcp_connect :: Ptr UVReq -> Ptr UVHandle -> Ptr SockAddr -> IO CInt

--------------------------------------------------------------------------------

uvPipeInit :: HasCallStack => Ptr UVLoop -> Ptr UVHandle -> IO ()
uvPipeInit loop handle = throwUVIfMinus_ $ uv_pipe_init loop handle 0
foreign import ccall unsafe uv_pipe_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt

--------------------------------------------------------------------------------

newtype UVTTYMode = UVTTYMode CInt
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVTTYMode, UVTTYMode,
    uV_TTY_MODE_NORMAL      = UV_TTY_MODE_NORMAL,
    uV_TTY_MODE_RAW         = UV_TTY_MODE_RAW,
    uV_TTY_MODE_IO          = UV_TTY_MODE_IO }

uvTTYInit :: HasCallStack => Ptr UVLoop -> Ptr UVHandle -> UVFD -> IO ()
uvTTYInit loop handle fd = throwUVIfMinus_ $ uv_tty_init loop handle (fromIntegral fd)
foreign import ccall unsafe uv_tty_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt

--------------------------------------------------------------------------------

type UVFSCallBack = FunPtr (Ptr UVReq -> IO ())

foreign import ccall "hs_uv.h &hs_uv_fs_callback" uvFSCallBack :: UVFSCallBack

newtype UVDirEntType = UVDirEntType CInt
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVDirEntType, UVDirEntType,
    uV_DIRENT_UNKNOWN = UV_DIRENT_UNKNOWN,
    uV_DIRENT_FILE    = UV_DIRENT_FILE,
    uV_DIRENT_DIR     = UV_DIRENT_DIR,
    uV_DIRENT_LINK    = UV_DIRENT_LINK,
    uV_DIRENT_FIFO    = UV_DIRENT_FIFO,
    uV_DIRENT_SOCKET  = UV_DIRENT_SOCKET,
    uV_DIRENT_CHAR    = UV_DIRENT_CHAR,
    uV_DIRENT_BLOCK   = UV_DIRENT_BLOCK}

data UVDirEnt

initUVDirEnt :: Resource (Ptr UVDirEnt)
initUVDirEnt = initResource hs_uv_dirent_alloc hs_uv_dirent_free
foreign import ccall unsafe hs_uv_dirent_alloc :: IO (Ptr UVDirEnt)
foreign import ccall unsafe hs_uv_dirent_free :: Ptr UVDirEnt -> IO ()

peekUVDirEnt :: Ptr UVDirEnt -> IO (CString, UVDirEntType)
peekUVDirEnt p = (,) 
    <$> (#{peek struct uv_dirent_s, name          } p)
    <*> (#{peek struct uv_dirent_s, type          } p)

uvFSScandir :: Ptr UVLoop -> Ptr UVReq -> CString -> Bool -> IO ()
uvFSScandir loop req path block = 
    throwUVIfMinus_ $ uv_fs_scandir loop req path 0 (if block then uvFSCallBack else nullFunPtr)
foreign import ccall unsafe uv_fs_scandir :: Ptr UVLoop -> Ptr UVReq -> CString -> CInt -> UVFSCallBack -> IO CInt
foreign import ccall unsafe uv_fs_scandir_next :: Ptr UVReq -> Ptr UVDirEnt -> IO CInt
