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
import System.IO.Resource
import Data.Bits
import System.IO.Net.SockAddr (SockAddr, SocketFamily(..))

#include "uv.h"
#include "hs_uv.h"

--------------------------------------------------------------------------------
-- Type alias
type UVSlot = Int
type UVFD = Int32

--------------------------------------------------------------------------------
-- CONSTANT
aCCEPT_BUFFER_SIZE = #const ACCEPT_BUFFER_SIZE
sO_REUSEPORT_LOAD_BALANCE = #const SO_REUSEPORT_LOAD_BALANCE

--------------------------------------------------------------------------------
-- loop
data UVLoop
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

newtype UVRunMode = UVRunMode CInt 
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVRunMode, UVRunMode, 
  uV_RUN_DEFAULT = UV_RUN_DEFAULT,
  uV_RUN_ONCE    = UV_RUN_ONCE,
  uV_RUN_NOWAIT  = UV_RUN_NOWAIT}

-- | Peek loop data pointer from uv loop  pointer.
peekUVLoopData :: Ptr UVLoop -> IO (Ptr UVLoopData)
peekUVLoopData p = #{peek uv_loop_t, data} p

foreign import ccall unsafe hs_uv_loop_init      :: CSize -> IO (Ptr UVLoop)
foreign import ccall unsafe hs_uv_loop_close     :: Ptr UVLoop -> IO ()

-- | uv_run with usafe FFI.
foreign import ccall unsafe uv_run            :: Ptr UVLoop -> UVRunMode -> IO CInt

-- | uv_run with safe FFI.
foreign import ccall safe "uv_run" uv_run_safe :: Ptr UVLoop -> UVRunMode -> IO CInt

foreign import ccall unsafe uv_loop_alive :: Ptr UVLoop -> IO CInt

--------------------------------------------------------------------------------
-- handle
data UVHandle

peekUVHandleData :: Ptr UVHandle -> IO UVSlot
peekUVHandleData p =  fromIntegral <$> (#{peek uv_handle_t, data} p :: IO CSize)

foreign import ccall unsafe hs_uv_fileno :: Ptr UVHandle -> IO UVFD
foreign import ccall unsafe hs_uv_handle_alloc  :: UVHandleType -> Ptr UVLoop -> IO (Ptr UVHandle)
foreign import ccall unsafe hs_uv_handle_free :: Ptr UVHandle -> IO ()
foreign import ccall unsafe hs_uv_handle_close :: Ptr UVHandle -> IO ()
foreign import ccall unsafe hs_uv_handle_alloc_no_slot  :: UVHandleType -> IO (Ptr UVHandle)
foreign import ccall unsafe hs_uv_handle_free_no_slot :: Ptr UVHandle -> IO ()
foreign import ccall unsafe hs_uv_handle_close_no_slot :: Ptr UVHandle -> IO ()

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
-- request

data UVReq

peekUVReqData :: Ptr UVReq -> IO UVSlot
peekUVReqData p = fromIntegral <$> (#{peek uv_req_t, data} p :: IO CSize)

foreign import ccall unsafe hs_uv_req_alloc :: UVReqType -> Ptr UVLoop -> IO (Ptr UVReq)
foreign import ccall unsafe hs_uv_req_free :: Ptr UVReq -> Ptr UVLoop -> IO ()
foreign import ccall unsafe hs_uv_req_alloc_no_slot :: UVReqType -> IO (Ptr UVReq)
foreign import ccall unsafe hs_uv_req_free_no_slot :: Ptr UVReq -> IO ()

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
-- thread safe wake up

foreign import ccall unsafe hs_uv_async_wake_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_async_send :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe hs_uv_timer_wake_start :: Ptr UVHandle -> Word64 -> IO CInt

--------------------------------------------------------------------------------
-- timer 

foreign import ccall unsafe uv_timer_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_timer_start :: Ptr UVHandle -> Word64 -> Word64 -> IO CInt
foreign import ccall unsafe uv_timer_again :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe  uv_timer_set_repeat :: Ptr UVHandle -> Word64 -> IO CInt 
foreign import ccall unsafe uv_timer_stop :: Ptr UVHandle -> IO CInt

--------------------------------------------------------------------------------
-- tcp
foreign import ccall unsafe hs_uv_tcp_open :: Ptr UVHandle -> UVFD -> IO CInt
foreign import ccall unsafe uv_tcp_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_tcp_init_ex :: Ptr UVLoop -> Ptr UVHandle -> CUInt -> IO CInt

foreign import ccall unsafe uv_tcp_nodelay :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_tcp_keepalive :: Ptr UVHandle -> CInt -> CUInt -> IO CInt

uV_TCP_IPV6ONLY :: CUInt
uV_TCP_IPV6ONLY = #{const UV_TCP_IPV6ONLY}
foreign import ccall unsafe uv_tcp_bind :: Ptr UVHandle -> Ptr SockAddr -> CUInt -> IO CInt
foreign import ccall unsafe hs_uv_tcp_connect :: Ptr UVReq -> Ptr UVHandle -> Ptr SockAddr -> IO CInt
foreign import ccall unsafe hs_uv_listen  :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe hs_uv_accept_check_init :: Ptr UVLoop -> Ptr UVHandle -> Ptr UVHandle -> IO CInt
foreign import ccall unsafe "hs_uv_listen_resume" uvListenResume :: Ptr UVHandle -> IO ()
foreign import ccall unsafe hs_set_socket_reuse :: Ptr UVHandle -> IO CInt

--------------------------------------------------------------------------------
-- pipe
foreign import ccall unsafe uv_pipe_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt

--------------------------------------------------------------------------------
-- stream
foreign import ccall unsafe hs_uv_read_start :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe hs_uv_write :: Ptr UVReq -> Ptr UVHandle -> IO CInt

--------------------------------------------------------------------------------
-- tty
newtype UVTTYMode = UVTTYMode CInt
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVTTYMode, UVTTYMode,
    uV_TTY_MODE_NORMAL      = UV_TTY_MODE_NORMAL,
    uV_TTY_MODE_RAW         = UV_TTY_MODE_RAW,
    uV_TTY_MODE_IO          = UV_TTY_MODE_IO }

foreign import ccall unsafe uv_tty_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt

--------------------------------------------------------------------------------
-- fs

foreign import ccall uv_fs_req_cleanup :: Ptr UVReq -> IO () 

type UVFSCallBack = FunPtr (Ptr UVReq -> IO ())

foreign import ccall "hs_uv.h &hs_uv_fs_callback" uvFSCallBack :: UVFSCallBack

newtype UVFileFlag = UVFileFlag CInt
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVFileFlag, UVFileFlag,
    uV_FS_O_APPEND       = UV_FS_O_APPEND,
    uV_FS_O_CREAT        = UV_FS_O_CREAT,
    uV_FS_O_DIRECT       = UV_FS_O_DIRECT,
    uV_FS_O_DIRECTORY    = UV_FS_O_DIRECTORY,
    uV_FS_O_DSYNC        = UV_FS_O_DSYNC,
    uV_FS_O_EXCL         = UV_FS_O_EXCL,
    uV_FS_O_EXLOCK       = UV_FS_O_EXLOCK,
    uV_FS_O_NOATIME      = UV_FS_O_NOATIME,
    uV_FS_O_NOCTTY       = UV_FS_O_NOCTTY,
    uV_FS_O_NOFOLLOW     = UV_FS_O_NOFOLLOW,
    uV_FS_O_NONBLOCK     = UV_FS_O_NONBLOCK,
    uV_FS_O_RDONLY       = UV_FS_O_RDONLY,
    uV_FS_O_RDWR         = UV_FS_O_RDWR,
    uV_FS_O_SYMLINK      = UV_FS_O_SYMLINK,
    uV_FS_O_SYNC         = UV_FS_O_SYNC,
    uV_FS_O_TRUNC        = UV_FS_O_TRUNC,
    uV_FS_O_WRONLY       = UV_FS_O_WRONLY,
    uV_FS_O_RANDOM       = UV_FS_O_RANDOM,
    uV_FS_O_SHORT_LIVED  = UV_FS_O_SHORT_LIVED,
    uV_FS_O_SEQUENTIAL   = UV_FS_O_SEQUENTIAL,
    uV_FS_O_TEMPORARY    = UV_FS_O_TEMPORARY}

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

peekUVDirEnt :: Ptr UVDirEnt -> IO (CString, UVDirEntType)
peekUVDirEnt p = (,) 
    <$> (#{peek struct uv_dirent_s, name          } p)
    <*> (#{peek struct uv_dirent_s, type          } p)

foreign import ccall unsafe hs_uv_dirent_alloc :: IO (Ptr UVDirEnt)
foreign import ccall unsafe hs_uv_dirent_free :: Ptr UVDirEnt -> IO ()

foreign import ccall unsafe uv_fs_scandir :: Ptr UVLoop -> Ptr UVReq -> CString -> CInt -> UVFSCallBack -> IO CInt
foreign import ccall unsafe uv_fs_scandir_next :: Ptr UVReq -> Ptr UVDirEnt -> IO CInt
