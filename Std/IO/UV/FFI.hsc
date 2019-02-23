{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UnliftedFFITypes           #-}

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
import           GHC.Prim
import           Std.Foreign.PrimArray
import           Std.IO.Exception
import           Std.IO.SockAddr    (SockAddr, SocketFamily (..))
import           System.Posix.Types (CSsize (..))
import           GHC.Generics

#include "hs_uv.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

--------------------------------------------------------------------------------
-- libuv version
foreign import ccall unsafe uv_version :: IO CUInt
foreign import ccall unsafe uv_version_string :: IO CString

--------------------------------------------------------------------------------
-- Type alias
type UVSlot = Int
-- | UVSlotUnSafe wrap a slot which may not have a 'MVar' in blocking table, 
--   i.e. the blocking table need to be resized.
newtype UVSlotUnSafe = UVSlotUnSafe { unsafeGetSlot :: UVSlot }
type UVFD = Int32

--------------------------------------------------------------------------------
-- CONSTANT

pattern ACCEPT_BUFFER_SIZE :: Int
pattern ACCEPT_BUFFER_SIZE = #const ACCEPT_BUFFER_SIZE
pattern SO_REUSEPORT_LOAD_BALANCE :: Int
pattern SO_REUSEPORT_LOAD_BALANCE = #const SO_REUSEPORT_LOAD_BALANCE
pattern INIT_LOOP_SIZE :: Int
pattern INIT_LOOP_SIZE = #const INIT_LOOP_SIZE

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
    deriving (Eq, Ord, Read, Show, FiniteBits, Bits, Storable, Num)

pattern UV_RUN_DEFAULT :: UVRunMode
pattern UV_RUN_DEFAULT = UVRunMode #{const UV_RUN_DEFAULT}
pattern UV_RUN_ONCE :: UVRunMode
pattern UV_RUN_ONCE    = UVRunMode #{const UV_RUN_ONCE}
pattern UV_RUN_NOWAIT :: UVRunMode
pattern UV_RUN_NOWAIT  = UVRunMode #{const UV_RUN_NOWAIT}

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
foreign import ccall unsafe uv_read_stop :: Ptr UVHandle -> IO CInt
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
-- udp
foreign import ccall unsafe uv_udp_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_udp_init_ex :: Ptr UVLoop -> Ptr UVHandle -> CUInt -> IO CInt
foreign import ccall unsafe uv_udp_open :: Ptr UVHandle -> UVFD -> IO CInt
foreign import ccall unsafe uv_udp_bind :: Ptr UVHandle -> Ptr SockAddr -> UVUDPFlag -> IO CInt

newtype UVMembership = UVMembership CInt deriving (Show, Eq, Ord)
pattern UV_LEAVE_GROUP = UVMembership #{const UV_LEAVE_GROUP}
pattern UV_JOIN_GROUP = UVMembership #{const UV_JOIN_GROUP}

newtype UVUDPFlag = UVUDPFlag CInt deriving (Show, Eq, Ord, Storable, Bits, FiniteBits, Num)
pattern UV_UDP_DEFAULT = UVUDPFlag 0
pattern UV_UDP_IPV6ONLY = UVUDPFlag #{const UV_UDP_IPV6ONLY}
pattern UV_UDP_REUSEADDR = UVUDPFlag #{const UV_UDP_REUSEADDR}

pattern UV_UDP_PARTIAL :: Int32
pattern UV_UDP_PARTIAL = #{const UV_UDP_PARTIAL}

foreign import ccall unsafe uv_udp_set_membership ::
    Ptr UVHandle -> CString -> CString -> UVMembership -> IO CInt
foreign import ccall unsafe uv_udp_set_multicast_loop :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_udp_set_multicast_ttl :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_udp_set_multicast_interface :: Ptr UVHandle -> CString -> IO CInt
foreign import ccall unsafe uv_udp_set_broadcast :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_udp_set_ttl :: Ptr UVHandle -> CInt -> IO CInt

foreign import ccall unsafe hs_uv_udp_recv_start :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_udp_recv_stop :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe hs_uv_udp_send 
    :: Ptr UVHandle -> Ptr SockAddr -> Ptr Word8 -> Int -> IO UVSlotUnSafe


--------------------------------------------------------------------------------
-- pipe
foreign import ccall unsafe uv_pipe_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt

--------------------------------------------------------------------------------
-- tty

-- | Terminal mode.
--
-- When in 'UV_TTY_MODE_RAW' mode, input is always available character-by-character,
-- not including modifiers. Additionally, all special processing of characters by the terminal is disabled, 
-- including echoing input characters. Note that CTRL+C will no longer cause a SIGINT when in this mode.
newtype UVTTYMode = UVTTYMode CInt
    deriving (Eq, Ord, Read, Show, FiniteBits, Bits, Storable, Num)

pattern UV_TTY_MODE_NORMAL :: UVTTYMode
pattern UV_TTY_MODE_NORMAL = UVTTYMode #{const UV_TTY_MODE_NORMAL}
pattern UV_TTY_MODE_RAW :: UVTTYMode
pattern UV_TTY_MODE_RAW = UVTTYMode #{const UV_TTY_MODE_RAW}
pattern UV_TTY_MODE_IO :: UVTTYMode
pattern UV_TTY_MODE_IO = UVTTYMode #{const UV_TTY_MODE_IO}

foreign import ccall unsafe uv_tty_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_tty_set_mode :: Ptr UVHandle -> UVTTYMode -> IO CInt

--------------------------------------------------------------------------------
-- fs

newtype UVFileMode = UVFileMode CInt
    deriving (Eq, Ord, Read, Show, FiniteBits, Bits, Storable, Num)

-- | 00700 user (file owner) has read, write and execute permission
pattern S_IRWXU :: UVFileMode
pattern S_IRWXU = UVFileMode #{const S_IRWXU}

-- | 00400 user has read permission
pattern S_IRUSR :: UVFileMode
pattern S_IRUSR = UVFileMode #{const S_IRUSR}

-- | 00200 user has write permission
pattern S_IWUSR :: UVFileMode
pattern S_IWUSR = UVFileMode #{const S_IWUSR}

-- | 00100 user has execute permission
pattern S_IXUSR :: UVFileMode
pattern S_IXUSR = UVFileMode #{const S_IXUSR}

-- | 00070 group has read, write and execute permission
pattern S_IRWXG :: UVFileMode
pattern S_IRWXG = UVFileMode #{const S_IRWXG}

-- | 00040 group has read permission
pattern S_IRGRP :: UVFileMode
pattern S_IRGRP = UVFileMode #{const S_IRGRP}

-- | 00020 group has write permission
pattern S_IWGRP :: UVFileMode
pattern S_IWGRP = UVFileMode #{const S_IWGRP}

-- | 00010 group has execute permission
pattern S_IXGRP :: UVFileMode
pattern S_IXGRP = UVFileMode #{const S_IXGRP}

-- | 00007 others have read, write and execute permission
pattern S_IRWXO :: UVFileMode
pattern S_IRWXO = UVFileMode #{const S_IRWXO}

-- | 00004 others have read permission
pattern S_IROTH :: UVFileMode
pattern S_IROTH = UVFileMode #{const S_IROTH}

-- | 00002 others have write permission
pattern S_IWOTH :: UVFileMode
pattern S_IWOTH = UVFileMode #{const S_IWOTH}

-- | 00001 others have execute permission
pattern S_IXOTH :: UVFileMode
pattern S_IXOTH = UVFileMode #{const S_IXOTH}

-- | Default mode for open, 0o666(readable and writable).
pattern DEFAULT_MODE :: UVFileMode
pattern DEFAULT_MODE = UVFileMode 0o666

-- non-threaded functions
foreign import ccall unsafe hs_uv_fs_open    :: CString -> UVFileFlag -> UVFileMode -> IO UVFD
foreign import ccall unsafe hs_uv_fs_close   :: UVFD -> IO Int
foreign import ccall unsafe hs_uv_fs_read    :: UVFD -> Ptr Word8 -> Int -> Int64 -> IO Int
foreign import ccall unsafe hs_uv_fs_write   :: UVFD -> Ptr Word8 -> Int -> Int64 -> IO Int
foreign import ccall unsafe hs_uv_fs_unlink  :: CString -> IO Int
foreign import ccall unsafe hs_uv_fs_mkdir   :: CString -> UVFileMode -> IO Int
foreign import ccall unsafe hs_uv_fs_rmdir   :: CString -> IO Int
foreign import ccall unsafe hs_uv_fs_mkdtemp :: CString -> Int -> CString -> IO Int

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
foreign import ccall unsafe hs_uv_fs_rmdir_threaded 
    :: CString -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_mkdtemp_threaded 
    :: CString -> Int -> CString -> Ptr UVLoop -> IO UVSlotUnSafe

newtype UVFileFlag = UVFileFlag CInt
    deriving (Eq, Ord, Read, Show, FiniteBits, Bits, Storable, Num)

-- | The file is opened in append mode. Before each write, the file offset is positioned at the end of the file.
pattern O_APPEND :: UVFileFlag
pattern O_APPEND = UVFileFlag #{const UV_FS_O_APPEND}

-- | The file is created if it does not already exist.
pattern O_CREAT :: UVFileFlag
pattern O_CREAT = UVFileFlag #{const UV_FS_O_CREAT}

-- | File IO is done directly to and from user-space buffers, which must be aligned. Buffer size and address should be a multiple of the physical sector size of the block device, (DO NOT USE WITH stdio's @BufferedIO@)
pattern O_DIRECT :: UVFileFlag
pattern O_DIRECT = UVFileFlag #{const UV_FS_O_DIRECT}

-- | If the path is not a directory, fail the open. (Not useful on regular file)
--
-- Note 'o_DIRECTORY' is not supported on Windows.
pattern O_DIRECTORY :: UVFileFlag
pattern O_DIRECTORY = UVFileFlag #{const UV_FS_O_DIRECTORY}

-- |The file is opened for synchronous IO. Write operations will complete once all data and a minimum of metadata are flushed to disk.
--
-- Note 'o_DSYNC' is supported on Windows via @FILE_FLAG_WRITE_THROUGH@.
pattern O_DSYNC :: UVFileFlag
pattern O_DSYNC = UVFileFlag #{const UV_FS_O_DSYNC}

-- | If the 'o_CREAT' flag is set and the file already exists, fail the open.
--
-- Note In general, the behavior of 'o_EXCL' is undefined if it is used without 'o_CREAT'. There is one exception: on 
-- Linux 2.6 and later, 'o_EXCL' can be used without 'o_CREAT' if pathname refers to a block device. If the block 
-- device is in use by the system (e.g., mounted), the open will fail with the error @EBUSY@.
pattern O_EXCL :: UVFileFlag
pattern O_EXCL = UVFileFlag #{const UV_FS_O_EXCL}

-- | Atomically obtain an exclusive lock.
--
-- Note UV_FS_O_EXLOCK is only supported on macOS and Windows.
-- (libuv: Changed in version 1.17.0: support is added for Windows.)
pattern O_EXLOCK :: UVFileFlag
pattern O_EXLOCK = UVFileFlag #{const UV_FS_O_EXLOCK}

-- | Do not update the file access time when the file is read.
-- 
-- Note 'o_NOATIME' is not supported on Windows.
pattern O_NOATIME :: UVFileFlag
pattern O_NOATIME = UVFileFlag #{const UV_FS_O_NOATIME}

-- | If the path identifies a terminal device, opening the path will not cause that terminal to become the controlling terminal for the process (if the process does not already have one). (Not sure if this flag is useful)
--
-- Note 'o_NOCTTY' is not supported on Windows.
pattern O_NOCTTY :: UVFileFlag
pattern O_NOCTTY = UVFileFlag #{const UV_FS_O_NOCTTY}

-- | If the path is a symbolic link, fail the open.
--
-- Note 'o_NOFOLLOW' is not supported on Windows.
pattern O_NOFOLLOW :: UVFileFlag
pattern O_NOFOLLOW = UVFileFlag #{const UV_FS_O_NOFOLLOW}

-- | Open the file in nonblocking mode if possible. (Definitely not useful with stdio)
--
-- Note 'o_NONBLOCK' is not supported on Windows. (Not useful on regular file anyway)
pattern O_NONBLOCK :: UVFileFlag
pattern O_NONBLOCK = UVFileFlag #{const UV_FS_O_NONBLOCK}

-- | Access is intended to be random. The system can use this as a hint to optimize file caching.
-- 
-- Note 'o_RANDOM' is only supported on Windows via @FILE_FLAG_RANDOM_ACCESS@.
pattern O_RANDOM :: UVFileFlag
pattern O_RANDOM = UVFileFlag #{const UV_FS_O_RANDOM}

-- | Open the file for read-only access.
pattern O_RDONLY :: UVFileFlag
pattern O_RDONLY = UVFileFlag #{const UV_FS_O_RDONLY}

-- | Open the file for read-write access.
pattern O_RDWR :: UVFileFlag
pattern O_RDWR = UVFileFlag #{const UV_FS_O_RDWR}


-- | Access is intended to be sequential from beginning to end. The system can use this as a hint to optimize file caching.
-- 
-- Note 'o_SEQUENTIAL' is only supported on Windows via @FILE_FLAG_SEQUENTIAL_SCAN@.
pattern O_SEQUENTIAL :: UVFileFlag
pattern O_SEQUENTIAL = UVFileFlag #{const UV_FS_O_SEQUENTIAL}

-- | The file is temporary and should not be flushed to disk if possible.
--
-- Note 'o_SHORT_LIVED' is only supported on Windows via @FILE_ATTRIBUTE_TEMPORARY@.
pattern O_SHORT_LIVED :: UVFileFlag
pattern O_SHORT_LIVED = UVFileFlag #{const UV_FS_O_SHORT_LIVED}

-- | Open the symbolic link itself rather than the resource it points to.
pattern O_SYMLINK :: UVFileFlag
pattern O_SYMLINK = UVFileFlag #{const UV_FS_O_SYMLINK}

-- | The file is opened for synchronous IO. Write operations will complete once all data and all metadata are flushed to disk.
--
-- Note 'o_SYNC' is supported on Windows via @FILE_FLAG_WRITE_THROUGH@.
pattern O_SYNC :: UVFileFlag
pattern O_SYNC = UVFileFlag #{const UV_FS_O_SYNC}

-- | The file is temporary and should not be flushed to disk if possible.
--
-- Note 'o_TEMPORARY' is only supported on Windows via @FILE_ATTRIBUTE_TEMPORARY@.
pattern O_TEMPORARY :: UVFileFlag
pattern O_TEMPORARY = UVFileFlag #{const UV_FS_O_TEMPORARY}

-- | If the file exists and is a regular file, and the file is opened successfully for write access, its length shall be truncated to zero.
pattern O_TRUNC :: UVFileFlag
pattern O_TRUNC = UVFileFlag #{const UV_FS_O_TRUNC}

-- | Open the file for write-only access.
pattern O_WRONLY :: UVFileFlag
pattern O_WRONLY = UVFileFlag #{const UV_FS_O_WRONLY}

#if defined(_WIN32)
newtype UVDirEntType = UVDirEntType CInt
#else
newtype UVDirEntType = UVDirEntType CChar
#endif
    deriving (Eq, Ord, Read, Show, FiniteBits, Bits, Storable, Num)

data DirEntType
    = DirEntUnknown
    | DirEntFile
    | DirEntDir
    | DirEntLink
    | DirEntFIFO
    | DirEntSocket
    | DirEntChar
    | DirEntBlock
  deriving (Read, Show, Eq, Ord, Generic)

fromUVDirEntType :: UVDirEntType -> DirEntType
fromUVDirEntType t
    | t == uV__DT_FILE   = DirEntFile
    | t == uV__DT_DIR    = DirEntDir
    | t == uV__DT_LINK   = DirEntLink
    | t == uV__DT_FIFO   = DirEntFIFO
    | t == uV__DT_SOCKET = DirEntSocket
    | t == uV__DT_CHAR   = DirEntChar
    | t == uV__DT_BLOCK  = DirEntBlock
    | otherwise          = DirEntUnknown

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

foreign import ccall unsafe hs_uv_fs_scandir_cleanup
    :: Ptr (Ptr UVDirEnt) -> Int -> IO ()
foreign import ccall unsafe hs_uv_fs_scandir
    :: CString -> MBA## (Ptr UVDirEnt) -> IO Int
foreign import ccall unsafe hs_uv_fs_scandir_extra_cleanup 
    :: Ptr (Ptr (Ptr UVDirEnt)) -> Int -> IO ()
foreign import ccall unsafe hs_uv_fs_scandir_threaded
    :: CString -> Ptr (Ptr (Ptr UVDirEnt)) -> Ptr UVLoop -> IO UVSlotUnSafe

data UVTimeSpec = UVTimeSpec 
    { uvtSecond     :: {-# UNPACK #-} !CLong
    , uvtNanoSecond :: {-# UNPACK #-} !CLong
    } deriving (Show, Read, Eq, Ord, Generic)

instance Storable UVTimeSpec where
    sizeOf _  = #{size uv_timespec_t}
    alignment _ = #{alignment uv_timespec_t}
    peek p = UVTimeSpec <$> (#{peek uv_timespec_t, tv_sec } p)
                        <*> (#{peek uv_timespec_t, tv_nsec } p)
    poke p (UVTimeSpec sec nsec) = do
        (#{poke uv_timespec_t, tv_sec  } p sec)
        (#{poke uv_timespec_t, tv_nsec } p nsec)

data UVStat = UVStat
    { stDev      :: {-# UNPACK #-} !Word64
    , stMode     :: {-# UNPACK #-} !Word64
    , stNlink    :: {-# UNPACK #-} !Word64
    , stUid      :: {-# UNPACK #-} !Word64
    , stGid      :: {-# UNPACK #-} !Word64
    , stRdev     :: {-# UNPACK #-} !Word64
    , stIno      :: {-# UNPACK #-} !Word64
    , stSize     :: {-# UNPACK #-} !Word64
    , stBlksize  :: {-# UNPACK #-} !Word64
    , stBlocks   :: {-# UNPACK #-} !Word64
    , stFlags    :: {-# UNPACK #-} !Word64
    , stGen      :: {-# UNPACK #-} !Word64
    , stAtim     :: {-# UNPACK #-} !UVTimeSpec
    , stMtim     :: {-# UNPACK #-} !UVTimeSpec
    , stCtim     :: {-# UNPACK #-} !UVTimeSpec
    , stBirthtim :: {-# UNPACK #-} !UVTimeSpec
    } deriving (Show, Read, Eq, Ord, Generic)

uvStatSize :: Int
uvStatSize = #{size uv_stat_t}

peekUVStat :: Ptr UVStat -> IO UVStat
peekUVStat p = UVStat
    <$> (#{peek uv_stat_t, st_dev          } p)
    <*> (#{peek uv_stat_t, st_mode         } p)
    <*> (#{peek uv_stat_t, st_nlink        } p)
    <*> (#{peek uv_stat_t, st_uid          } p)
    <*> (#{peek uv_stat_t, st_gid          } p)
    <*> (#{peek uv_stat_t, st_rdev         } p)
    <*> (#{peek uv_stat_t, st_ino          } p)
    <*> (#{peek uv_stat_t, st_size         } p)
    <*> (#{peek uv_stat_t, st_blksize      } p)
    <*> (#{peek uv_stat_t, st_blocks       } p)
    <*> (#{peek uv_stat_t, st_flags        } p)
    <*> (#{peek uv_stat_t, st_gen          } p)
    <*> (#{peek uv_stat_t, st_atim         } p)
    <*> (#{peek uv_stat_t, st_mtim         } p)
    <*> (#{peek uv_stat_t, st_ctim         } p)
    <*> (#{peek uv_stat_t, st_birthtim     } p)

foreign import ccall unsafe hs_uv_fs_stat :: CString -> Ptr UVStat -> IO Int
foreign import ccall unsafe hs_uv_fs_fstat :: UVFD -> Ptr UVStat -> IO Int
foreign import ccall unsafe hs_uv_fs_lstat :: CString -> Ptr UVStat -> IO Int
foreign import ccall unsafe hs_uv_fs_rename :: CString -> CString -> IO Int
foreign import ccall unsafe hs_uv_fs_fsync :: UVFD -> IO Int
foreign import ccall unsafe hs_uv_fs_fdatasync :: UVFD -> IO Int
foreign import ccall unsafe hs_uv_fs_ftruncate :: UVFD -> Int64 -> IO Int

foreign import ccall unsafe hs_uv_fs_stat_threaded
    :: CString -> Ptr UVStat -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_fstat_threaded
    :: UVFD -> Ptr UVStat -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_lstat_threaded
    :: CString -> Ptr UVStat -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_rename_threaded
    :: CString -> CString -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_fsync_threaded
    :: UVFD -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_fdatasync_threaded
    :: UVFD -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_ftruncate_threaded 
    :: UVFD -> Int64 -> Ptr UVLoop -> IO UVSlotUnSafe

-- | Flags control copying.
-- 
--   * 'COPYFILE_EXCL': If present, uv_fs_copyfile() will fail with UV_EEXIST if the destination path already exists. The default behavior is to overwrite the destination if it exists.
--   * 'COPYFILE_FICLONE': If present, uv_fs_copyfile() will attempt to create a copy-on-write reflink. If the underlying platform does not support copy-on-write, then a fallback copy mechanism is used.
-- 
newtype UVCopyFileFlag = UVCopyFileFlag CInt
    deriving (Eq, Ord, Read, Show, FiniteBits, Bits, Storable, Num)

pattern COPYFILE_DEFAULT :: UVCopyFileFlag
pattern COPYFILE_DEFAULT = UVCopyFileFlag 0

pattern COPYFILE_EXCL :: UVCopyFileFlag
pattern COPYFILE_EXCL = UVCopyFileFlag #{const UV_FS_COPYFILE_EXCL}

pattern COPYFILE_FICLONE :: UVCopyFileFlag
#ifdef UV_FS_COPYFILE_FICLONE
pattern COPYFILE_FICLONE = UVCopyFileFlag #{const UV_FS_COPYFILE_FICLONE}
#else
pattern COPYFILE_FICLONE = UVCopyFileFlag 0   -- fallback to normal copy.
#endif

foreign import ccall unsafe hs_uv_fs_copyfile :: CString -> CString -> UVCopyFileFlag -> IO Int
foreign import ccall unsafe hs_uv_fs_copyfile_threaded
    :: CString -> CString -> UVCopyFileFlag -> Ptr UVLoop -> IO UVSlotUnSafe

newtype UVAccessMode = UVAccessMode CInt
    deriving (Eq, Ord, Read, Show, FiniteBits, Bits, Storable, Num)

pattern F_OK :: UVAccessMode
pattern F_OK = UVAccessMode #{const F_OK}
pattern R_OK :: UVAccessMode
pattern R_OK = UVAccessMode #{const R_OK}
pattern W_OK :: UVAccessMode
pattern W_OK = UVAccessMode #{const W_OK}
pattern X_OK :: UVAccessMode
pattern X_OK = UVAccessMode #{const X_OK}

data AccessResult = NoExistence | NoPermission | AccessOK deriving (Show, Eq, Ord)

foreign import ccall unsafe hs_uv_fs_access :: CString -> UVAccessMode -> IO Int
foreign import ccall unsafe hs_uv_fs_access_threaded
    :: CString -> UVAccessMode -> Ptr UVLoop -> IO UVSlotUnSafe

foreign import ccall unsafe hs_uv_fs_chmod :: CString -> UVFileMode -> IO Int
foreign import ccall unsafe hs_uv_fs_chmod_threaded
    :: CString -> UVFileMode -> Ptr UVLoop -> IO UVSlotUnSafe

foreign import ccall unsafe hs_uv_fs_fchmod :: UVFD -> UVFileMode -> IO Int
foreign import ccall unsafe hs_uv_fs_fchmod_threaded
    :: UVFD -> UVFileMode -> Ptr UVLoop -> IO UVSlotUnSafe

foreign import ccall unsafe hs_uv_fs_utime :: CString -> Double -> Double -> IO Int
foreign import ccall unsafe hs_uv_fs_utime_threaded
    :: CString -> Double -> Double -> Ptr UVLoop -> IO UVSlotUnSafe

foreign import ccall unsafe hs_uv_fs_futime :: UVFD -> Double -> Double -> IO Int
foreign import ccall unsafe hs_uv_fs_futime_threaded
    :: UVFD -> Double -> Double -> Ptr UVLoop -> IO UVSlotUnSafe

newtype UVSymlinkFlag = UVSymlinkFlag CInt
    deriving (Eq, Ord, Read, Show, FiniteBits, Bits, Storable, Num)

pattern SYMLINK_DEFAULT :: UVSymlinkFlag
pattern SYMLINK_DEFAULT = UVSymlinkFlag 0

pattern SYMLINK_DIR :: UVSymlinkFlag
pattern SYMLINK_DIR = UVSymlinkFlag #{const UV_FS_SYMLINK_DIR}

pattern SYMLINK_JUNCTION :: UVSymlinkFlag
pattern SYMLINK_JUNCTION = UVSymlinkFlag #{const UV_FS_SYMLINK_JUNCTION}

foreign import ccall unsafe hs_uv_fs_link :: CString -> CString -> IO Int
foreign import ccall unsafe hs_uv_fs_link_threaded
    :: CString -> CString -> Ptr UVLoop -> IO UVSlotUnSafe

foreign import ccall unsafe hs_uv_fs_symlink :: CString -> CString -> UVSymlinkFlag -> IO Int
foreign import ccall unsafe hs_uv_fs_symlink_threaded
    :: CString -> CString -> UVSymlinkFlag -> Ptr UVLoop -> IO UVSlotUnSafe

-- readlink and realpath share the same cleanup and callback
foreign import ccall unsafe hs_uv_fs_readlink_cleanup
    :: CString -> IO ()
foreign import ccall unsafe hs_uv_fs_readlink
    :: CString -> MBA## CString -> IO Int
foreign import ccall unsafe hs_uv_fs_realpath
    :: CString -> MBA## CString -> IO Int
foreign import ccall unsafe hs_uv_fs_readlink_extra_cleanup 
    :: Ptr CString -> IO ()
foreign import ccall unsafe hs_uv_fs_readlink_threaded
    :: CString -> Ptr CString -> Ptr UVLoop -> IO UVSlotUnSafe
foreign import ccall unsafe hs_uv_fs_realpath_threaded
    :: CString -> Ptr CString -> Ptr UVLoop -> IO UVSlotUnSafe

--------------------------------------------------------------------------------
-- misc

newtype UVHandleType = UVHandleType CInt deriving (Eq, Ord, Read, Show, Storable)

pattern UV_UNKNOWN_HANDLE :: UVHandleType
pattern UV_UNKNOWN_HANDLE = UVHandleType #{const UV_UNKNOWN_HANDLE}
pattern UV_ASYNC :: UVHandleType
pattern UV_ASYNC = UVHandleType #{const UV_ASYNC}
pattern UV_CHECK :: UVHandleType
pattern UV_CHECK = UVHandleType #{const UV_CHECK}
pattern UV_FS_EVENT :: UVHandleType
pattern UV_FS_EVENT = UVHandleType #{const UV_FS_EVENT}
pattern UV_FS_POLL :: UVHandleType
pattern UV_FS_POLL = UVHandleType #{const UV_FS_POLL}
pattern UV_HANDLE :: UVHandleType
pattern UV_HANDLE = UVHandleType #{const UV_HANDLE}
pattern UV_IDLE :: UVHandleType
pattern UV_IDLE = UVHandleType #{const UV_IDLE}
pattern UV_NAMED_PIPE :: UVHandleType
pattern UV_NAMED_PIPE = UVHandleType #{const UV_NAMED_PIPE}
pattern UV_POLL :: UVHandleType
pattern UV_POLL = UVHandleType #{const UV_POLL}
pattern UV_PREPARE :: UVHandleType
pattern UV_PREPARE = UVHandleType #{const UV_PREPARE}
pattern UV_PROCESS :: UVHandleType
pattern UV_PROCESS = UVHandleType #{const UV_PROCESS}
pattern UV_STREAM :: UVHandleType
pattern UV_STREAM = UVHandleType #{const UV_STREAM}
pattern UV_TCP :: UVHandleType
pattern UV_TCP = UVHandleType #{const UV_TCP}
pattern UV_TIMER :: UVHandleType
pattern UV_TIMER = UVHandleType #{const UV_TIMER}
pattern UV_TTY :: UVHandleType
pattern UV_TTY = UVHandleType #{const UV_TTY}
pattern UV_UDP :: UVHandleType
pattern UV_UDP = UVHandleType #{const UV_UDP}
pattern UV_SIGNAL :: UVHandleType
pattern UV_SIGNAL = UVHandleType #{const UV_SIGNAL}
pattern UV_FILE :: UVHandleType
pattern UV_FILE = UVHandleType #{const UV_FILE}

foreign import ccall unsafe uv_guess_handle :: UVFD -> IO UVHandleType
