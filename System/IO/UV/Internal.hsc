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

#include "hs_uv.h"

--------------------------------------------------------------------------------
-- Type alias
type UVSlot = Int
newtype UVSlotUnSafe = UVSlotUnSafe { unsafeGetSlot :: UVSlot }
type UVFD = Int32

--------------------------------------------------------------------------------
-- CONSTANT
aCCEPT_BUFFER_SIZE :: Int
aCCEPT_BUFFER_SIZE = #const ACCEPT_BUFFER_SIZE
sO_REUSEPORT_LOAD_BALANCE :: Int
sO_REUSEPORT_LOAD_BALANCE = #const SO_REUSEPORT_LOAD_BALANCE
iNIT_LOOP_SIZE :: Int
iNIT_LOOP_SIZE = #const INIT_LOOP_SIZE

--------------------------------------------------------------------------------
-- loop
data UVLoop
data UVLoopData

peekUVEventQueue :: Ptr UVLoopData -> IO (Int, Ptr Int)
peekUVEventQueue p = (,)
    <$> (#{peek hs_loop_data, event_counter          } p)
    <*> (#{peek hs_loop_data, event_queue            } p)

clearUVEventCounter :: Ptr UVLoopData -> IO ()
clearUVEventCounter p = do
    #{poke hs_loop_data, event_counter          } p $ (0 :: Int)

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

foreign import ccall unsafe hs_uv_loop_init      :: Int -> IO (Ptr UVLoop)
foreign import ccall unsafe hs_uv_loop_close     :: Ptr UVLoop -> IO ()

-- | uv_run with usafe FFI.
foreign import ccall unsafe uv_run            :: Ptr UVLoop -> UVRunMode -> IO CInt

-- | uv_run with safe FFI.
foreign import ccall safe "uv_run" uv_run_safe :: Ptr UVLoop -> UVRunMode -> IO CInt

foreign import ccall unsafe uv_loop_alive :: Ptr UVLoop -> IO CInt

--------------------------------------------------------------------------------
-- thread safe wake up

foreign import ccall unsafe hs_uv_wake_up_timer :: Ptr UVLoopData -> IO CInt
foreign import ccall unsafe hs_uv_wake_up_async :: Ptr UVLoopData -> IO CInt

--------------------------------------------------------------------------------
-- handle
data UVHandle

peekUVHandleData :: Ptr UVHandle -> IO UVSlotUnSafe
peekUVHandleData p =  UVSlotUnSafe <$> (#{peek uv_handle_t, data} p :: IO Int)

foreign import ccall unsafe hs_uv_fileno :: Ptr UVHandle -> IO UVFD
foreign import ccall unsafe hs_uv_handle_alloc :: Ptr UVLoop -> IO (Ptr UVHandle)
foreign import ccall unsafe hs_uv_handle_free  :: Ptr UVHandle -> IO ()
foreign import ccall unsafe hs_uv_handle_close :: Ptr UVHandle -> IO ()

--------------------------------------------------------------------------------
-- request

-- foreign import ccall unsafe hs_uv_cancel :: Ptr UVLoop -> UVSlot -> IO ()

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
foreign import ccall unsafe hs_uv_tcp_connect :: Ptr UVHandle -> Ptr SockAddr -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_listen  :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe "hs_uv_listen_resume" uvListenResume :: Ptr UVHandle -> IO ()
foreign import ccall unsafe hs_set_socket_reuse :: Ptr UVHandle -> IO CInt

foreign import ccall unsafe hs_uv_accept_check_init :: Ptr UVHandle -> IO (Ptr UVHandle)
foreign import ccall unsafe hs_uv_accept_check_close :: Ptr UVHandle -> IO ()

--------------------------------------------------------------------------------
-- pipe
foreign import ccall unsafe uv_pipe_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt

--------------------------------------------------------------------------------
-- stream
foreign import ccall unsafe hs_uv_read_start :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe hs_uv_write :: Ptr UVHandle -> Ptr Word8 -> Int -> IO UVSlotUnSafe

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

type UVFileMode = Int32
newtype UVFileFlag = UVFileFlag CInt
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

-- non-threaded functions
foreign import ccall unsafe hs_uv_fs_open :: CString -> UVFileFlag -> UVFileMode -> IO UVFD
foreign import ccall unsafe hs_uv_fs_close :: UVFD -> IO CInt
foreign import ccall unsafe hs_uv_fs_read :: UVFD -> Ptr Word8 -> CInt -> CInt -> IO CInt
foreign import ccall unsafe hs_uv_fs_write :: UVFD -> Ptr Word8 -> CInt -> CInt -> IO CInt
foreign import ccall unsafe hs_uv_fs_unlink :: CString -> IO CInt
foreign import ccall unsafe hs_uv_fs_mkdir :: CString -> UVFileMode -> IO CInt

-- threaded functions
foreign import ccall unsafe hs_uv_fs_open_threaded 
    :: Ptr UVLoop -> CString -> UVFileFlag -> UVFileMode -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_close_threaded 
    :: Ptr UVLoop -> UVFD -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_read_threaded  
    :: Ptr UVLoop -> UVFD -> Ptr Word8 -> CInt -> CInt -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_write_threaded 
    :: Ptr UVLoop -> UVFD -> Ptr Word8 -> CInt -> CInt -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_unlink_threaded
    :: Ptr UVLoop -> CString -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_mkdir_threaded 
    :: Ptr UVLoop -> CString -> UVFileMode -> IO UVSlotUnSafe

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
{-
data UVDirEnt

initUVDirEnt :: Resource (Ptr UVDirEnt)
initUVDirEnt = initResource hs_uv_dirent_alloc hs_uv_dirent_free

peekUVDirEnt :: Ptr UVDirEnt -> IO (CString, UVDirEntType)
peekUVDirEnt p = (,) 
    <$> (#{peek struct uv_dirent_s, name          } p)
    <*> (#{peek struct uv_dirent_s, type          } p)

foreign import ccall unsafe hs_uv_dirent_alloc :: IO (Ptr UVDirEnt)
foreign import ccall unsafe hs_uv_dirent_free :: Ptr UVDirEnt -> IO ()

foreign import ccall unsafe uv_fs_scandir :: Ptr UVLoop -> CString -> CInt -> UVFSCallBack -> IO CInt
foreign import ccall unsafe uv_fs_scandir_next :: UVSlot -> Ptr UVDirEnt -> IO CInt
-}
