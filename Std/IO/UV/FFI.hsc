{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}

{-|
Module      : Std.IO.UV
Description : libuv operations
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

INTERNAL MODULE, provides all libuv side operations.

-}

module Std.IO.UV.FFI where

import           Data.Bits
import           Data.Int
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import           Std.Foreign.PrimArray
import           Std.IO.Exception
import           Std.IO.SockAddr    (SockAddr, SocketFamily (..))
import           System.Posix.Types (CSsize (..))

#include "hs_uv.h"

--------------------------------------------------------------------------------
-- Type alias
type UVSlot = Int
-- | UVSlotUnSafe wrap a slot which may not have a 'MVar' in blocking table, 
--   i.e. the blocking table need to be resized.
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
foreign import ccall unsafe "hs_uv_run" uv_run    :: Ptr UVLoop -> UVRunMode -> IO CInt

-- | uv_run with safe FFI.
foreign import ccall safe "hs_uv_run" uv_run_safe :: Ptr UVLoop -> UVRunMode -> IO CInt

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

foreign import ccall unsafe hs_uv_cancel :: Ptr UVLoop -> UVSlot -> IO ()

--------------------------------------------------------------------------------
-- stream

foreign import ccall unsafe hs_uv_listen  :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe hs_uv_listen_resume :: Ptr UVHandle -> IO ()

foreign import ccall unsafe hs_uv_read_start :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe hs_uv_write :: Ptr UVHandle -> Ptr Word8 -> Int -> IO UVSlotUnSafe

foreign import ccall unsafe hs_uv_accept_check_alloc :: Ptr UVHandle -> IO (Ptr UVHandle)
foreign import ccall unsafe hs_uv_accept_check_init :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe hs_uv_accept_check_close :: Ptr UVHandle -> IO ()

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
foreign import ccall unsafe hs_set_socket_reuse :: Ptr UVHandle -> IO CInt

--------------------------------------------------------------------------------
-- pipe
foreign import ccall unsafe uv_pipe_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt

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
foreign import ccall unsafe hs_uv_fs_open    :: CString -> UVFileFlag -> UVFileMode -> IO UVFD
foreign import ccall unsafe hs_uv_fs_close   :: UVFD -> IO CInt
foreign import ccall unsafe hs_uv_fs_read    :: UVFD -> Ptr Word8 -> Int -> Int64 -> IO Int
foreign import ccall unsafe hs_uv_fs_write   :: UVFD -> Ptr Word8 -> Int -> Int64 -> IO Int
foreign import ccall unsafe hs_uv_fs_unlink  :: CString -> IO CInt
foreign import ccall unsafe hs_uv_fs_mkdir   :: CString -> UVFileMode -> IO CInt
foreign import ccall unsafe hs_uv_fs_mkdtemp :: CString -> Int -> CString -> IO CInt

-- threaded functions
foreign import ccall unsafe hs_uv_fs_open_threaded 
    :: CString -> UVFileFlag -> UVFileMode -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_close_threaded 
    :: UVFD -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_read_threaded  
    :: UVFD -> Ptr Word8 -> Int -> Int64 -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_write_threaded 
    :: UVFD -> Ptr Word8 -> Int -> Int64 -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_unlink_threaded
    :: CString -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_mkdir_threaded 
    :: CString -> UVFileMode -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_mkdtemp_threaded 
    :: CString -> Int -> CString -> Ptr UVLoop -> IO UVSlotUnSafe

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

newtype UVDirEntType = UVDirEntType CChar
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVDirEntType, UVDirEntType,
    uV__DT_FILE    = UV__DT_FILE,
    uV__DT_DIR     = UV__DT_DIR,
    uV__DT_LINK    = UV__DT_LINK,
    uV__DT_FIFO    = UV__DT_FIFO,
    uV__DT_SOCKET  = UV__DT_SOCKET,
    uV__DT_CHAR    = UV__DT_CHAR,
    uV__DT_BLOCK   = UV__DT_BLOCK}

data UVDirEnt

peekUVDirEnt :: Ptr UVDirEnt -> IO (CString, UVDirEntType)
#ifdef HAVE_DIRENT_TYPES
peekUVDirEnt p = (,) (#{ptr hs_uv__dirent_t, d_name } p) <$> (#{peek hs_uv__dirent_t, d_type } p)
#else
peekUVDirEnt p = return ((#{ptr hs_uv__dirent_t,  d_name } p), #{const DT_UNKNOWN})
#endif

foreign import ccall unsafe hs_uv_fs_scandir_extra_cleanup :: Ptr (Ptr (Ptr UVDirEnt)) -> Int -> IO ()
foreign import ccall unsafe hs_uv_fs_scandir_cleanup :: Ptr (Ptr UVDirEnt) -> Int -> IO ()
foreign import ccall unsafe hs_uv_fs_scandir
    :: CString -> MutableByteArray## RealWorld -> IO Int
foreign import ccall unsafe hs_uv_fs_scandir_threaded
    :: CString -> Ptr (Ptr (Ptr UVDirEnt)) -> Ptr UVLoop -> IO UVSlotUnSafe
