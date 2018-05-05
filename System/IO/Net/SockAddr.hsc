{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : System.IO.Net.SockAddr
Description : TCP/UDP socket address API
Copyright   : (c) Winterland, 2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provides necessary types and constant for low level socket operation.

-}

module System.IO.Net.SockAddr
  ( -- * name to address
    SockAddr(..)
  , peekSockAddr
  , withSockAddr
   -- ** IPv4 address
  , InetAddr
  , inetAny
  , inetBroadcast
  , inetNone
  , inetLoopback
  , inetUnspecificGroup
  , inetAllHostsGroup
  , inetMaxLocalGroup
  , inetAddrToTuple
  , tupleToInetAddr
   -- ** IPv6 address
  , Inet6Addr
  , inet6Any
  , inet6Loopback
  , inet6AddrToTuple
  , tupleToInet6Addr
  , FlowInfo
  , ScopeID
  -- * port numbber
  , PortNumber 
  , aNY_PORT
  , htons
  , ntohs
  , ntohl
  , htonl
  -- * family, type, protocol
  , SocketFamily(..)
  , aF_UNSPEC
  , aF_INET
  , aF_INET6
  , SocketType(..)
  , sOCK_DGRAM
  , sOCK_STREAM
  , sOCK_SEQPACKET
  , sOCK_RAW
  , sOCK_RDM
  , SocketProtocol(..)
  , iPPROTO_TCP
  , iPPROTO_UDP
  ) where

import Foreign
import Foreign.C
import System.IO.Exception
import GHC.Stack.Compat
import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes)
import qualified Data.List as List
import Data.Bits
import Numeric (showHex)
import Data.Typeable
import Data.Ratio
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Data.Vector as V
import Data.CBytes
import Foreign.Marshal.Utils (copyBytes)
import Data.Primitive.PrimArray
import Control.Monad.Primitive

#include "hs_uv.h" 

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

#if defined(i386_HOST_ARCH) && defined(mingw32_HOST_OS)
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

--------------------------------------------------------------------------------

#if defined(_WIN32)
type CSaFamily = (#type unsigned short)
#elif defined(darwin_HOST_OS)
type CSaFamily = (#type u_char)
#else
type CSaFamily = (#type sa_family_t)
#endif

-- | IPv4 or IPv6 socket address, i.e. the `sockaddr_in` or `sockaddr_in6` struct.
-- 
data SockAddr 
    = SockAddrInet
        {-# UNPACK #-} !PortNumber  -- sin_port  (network byte order)
        {-# UNPACK #-} !InetAddr    -- sin_addr  (ditto)
    | SockAddrInet6
        {-# UNPACK #-} !PortNumber  -- sin6_port (network byte order)
        {-# UNPACK #-} !FlowInfo    -- sin6_flowinfo (ditto)
        {-# UNPACK #-} !Inet6Addr   -- sin6_addr (ditto)
        {-# UNPACK #-} !ScopeID     -- sin6_scope_id (ditto)
  deriving (Show, Eq, Ord)

type FlowInfo = Word32
type ScopeID = Word32

--------------------------------------------------------------------------------

-- | Independent of endianness. For example @127.0.0.1@ is stored as @(127, 0, 0, 1)@.
--
-- For direct manipulation prefer 'inetAddrToTuple' and 'tupleToInetAddr'.
--
newtype InetAddr = InetAddr Word32 deriving (Eq, Ord)
instance Show InetAddr where
    showsPrec _ ia = 
        let (a,b,c,d) = inetAddrToTuple ia
        in ("InetAddr " ++) . shows a . ('.':)
                            . shows b . ('.':)
                            . shows c . ('.':)
                            . shows d

-- | @0.0.0.0@
inetAny             :: InetAddr
inetAny              = InetAddr 0

-- | @255.255.255.255@
inetBroadcast       :: InetAddr
inetBroadcast        = tupleToInetAddr (255,255,255,255)

-- | @255.255.255.255@
inetNone            :: InetAddr
inetNone             = tupleToInetAddr (255,255,255,255)

-- | @127.0.0.1@
inetLoopback        :: InetAddr
inetLoopback         = tupleToInetAddr (127,  0,  0,  1)

-- | @224.0.0.0@
inetUnspecificGroup :: InetAddr
inetUnspecificGroup  = tupleToInetAddr (224,  0,  0,  0)

-- | @224.0.0.1@
inetAllHostsGroup   :: InetAddr
inetAllHostsGroup    = tupleToInetAddr (224,  0,  0,  1)

-- | @224.0.0.255@
inetMaxLocalGroup   :: InetAddr
inetMaxLocalGroup    = tupleToInetAddr (224,  0,  0,255)

instance Storable InetAddr where
    sizeOf _ = sizeOf (undefined :: Word32)
    alignment _ = alignment (undefined :: Word32) 
    peek p = (InetAddr . ntohl) `fmap` peekByteOff p 0
    poke p (InetAddr ia) = pokeByteOff p 0 (htonl ia)

-- | Converts 'HostAddress' to representation-independent IPv4 quadruple.
-- For example for @127.0.0.1@ the function will return @(127, 0, 0, 1)@
-- regardless of host endianness.
inetAddrToTuple :: InetAddr -> (Word8, Word8, Word8, Word8)
inetAddrToTuple (InetAddr ia) =
    let byte i = fromIntegral (ia `shiftR` i) :: Word8
    in (byte 24, byte 16, byte 8, byte 0)

-- | Converts IPv4 quadruple to 'HostAddress'.
tupleToInetAddr :: (Word8, Word8, Word8, Word8) -> InetAddr
tupleToInetAddr (b3, b2, b1, b0) =
    let x `sl` i = fromIntegral x `shiftL` i :: Word32
    in InetAddr $ (b3 `sl` 24) .|. (b2 `sl` 16) .|. (b1 `sl` 8) .|. (b0 `sl` 0)

--------------------------------------------------------------------------------

-- | Independent of endianness. For example @::1@ is stored as @(0, 0, 0, 1)@.
--
-- For direct manipulation prefer 'inet6AddrToTuple' and 'tupleToInet6Addr'.
--
data Inet6Addr = Inet6Addr {-# UNPACK #-}!Word32
                           {-# UNPACK #-}!Word32
                           {-# UNPACK #-}!Word32
                           {-# UNPACK #-}!Word32 deriving (Eq, Ord)

instance Show Inet6Addr where
    showsPrec _ i6a = 
        let (a,b,c,d,e,f,g,h) = inet6AddrToTuple i6a
        in ("Inet6Addr " ++) . showHex a . (':':)
                             . showHex b . (':':)
                             . showHex c . (':':)
                             . showHex d . (':':)
                             . showHex e . (':':)
                             . showHex f . (':':)
                             . showHex g . (':':)
                             . showHex h

-- | @::@
inet6Any      :: Inet6Addr
inet6Any       = Inet6Addr 0 0 0 0

-- | @::1@
inet6Loopback :: Inet6Addr
inet6Loopback  = Inet6Addr 0 0 0 1

inet6AddrToTuple :: Inet6Addr -> (Word16, Word16, Word16, Word16,
                                        Word16, Word16, Word16, Word16)
inet6AddrToTuple (Inet6Addr w3 w2 w1 w0) =
    let high, low :: Word32 -> Word16
        high w = fromIntegral (w `shiftR` 16)
        low w = fromIntegral w
    in (high w3, low w3, high w2, low w2, high w1, low w1, high w0, low w0)

tupleToInet6Addr :: (Word16, Word16, Word16, Word16,
                        Word16, Word16, Word16, Word16) -> Inet6Addr
tupleToInet6Addr (w7, w6, w5, w4, w3, w2, w1, w0) =
    let add :: Word16 -> Word16 -> Word32
        high `add` low = (fromIntegral high `shiftL` 16) .|. (fromIntegral low)
    in  Inet6Addr (w7 `add` w6) (w5 `add` w4) (w3 `add` w2) (w1 `add` w0)

instance Storable Inet6Addr where
    sizeOf _    = #size struct in6_addr
    alignment _ = #alignment struct in6_addr

    peek p = do
        a <- peek32 p 0
        b <- peek32 p 1
        c <- peek32 p 2
        d <- peek32 p 3
        return $ Inet6Addr a b c d

    poke p (Inet6Addr a b c d) = do
        poke32 p 0 a
        poke32 p 1 b
        poke32 p 2 c
        poke32 p 3 d

--------------------------------------------------------------------------------

peekSockAddr :: HasCallStack => Ptr SockAddr -> IO SockAddr
peekSockAddr p = do
    family <- (#peek struct sockaddr, sa_family) p
    case family :: CSaFamily of
        (#const AF_INET) -> do
            addr <- (#peek struct sockaddr_in, sin_addr) p
            port <- (#peek struct sockaddr_in, sin_port) p
            return (SockAddrInet (PortNum port) addr)
        (#const AF_INET6) -> do
            port <- (#peek struct sockaddr_in6, sin6_port) p
            flow <- (#peek struct sockaddr_in6, sin6_flowinfo) p
            addr <- (#peek struct sockaddr_in6, sin6_addr) p
            scope <- (#peek struct sockaddr_in6, sin6_scope_id) p
            return (SockAddrInet6 (PortNum port) flow addr scope)

        _ -> throwIO $ OtherError (IOEInfo "" "Unsupported net family" callStack)

withSockAddr :: SockAddr -> (Ptr SockAddr -> IO a) -> IO a
withSockAddr sa@(SockAddrInet (PortNum port) addr) f = do
    allocaBytesAligned (#size struct sockaddr_in) (#alignment struct sockaddr_in) $ \ p -> do
#if defined(darwin_HOST_OS)
        -- zeroMemory p (#const sizeof(struct sockaddr_in))
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
        (#poke struct sockaddr_in, sin_len) p ((#const sizeof(struct sockaddr_in)) :: Word8)
#endif
        (#poke struct sockaddr_in, sin_family) p ((#const AF_INET) :: CSaFamily)
        (#poke struct sockaddr_in, sin_port) p port
        (#poke struct sockaddr_in, sin_addr) p addr
        f p
withSockAddr sa@(SockAddrInet6 (PortNum port) flow addr scope) f = do
    allocaBytesAligned (#size struct sockaddr_in6) (#alignment struct sockaddr_in) $ \ p -> do
#if defined(darwin_HOST_OS)
        -- zeroMemory p (#const sizeof(struct sockaddr_in6))
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
        (#poke struct sockaddr_in6, sin6_len) p ((#const sizeof(struct sockaddr_in6)) :: Word8)
#endif
        (#poke struct sockaddr_in6, sin6_family) p ((#const AF_INET6) :: CSaFamily)
        (#poke struct sockaddr_in6, sin6_port) p port
        (#poke struct sockaddr_in6, sin6_flowinfo) p flow
        (#poke struct sockaddr_in6, sin6_addr) p (addr)
        (#poke struct sockaddr_in6, sin6_scope_id) p scope
        f p

-- The peek32 and poke32 functions work around the fact that the RFCs
-- don't require 32-bit-wide address fields to be present.  We can
-- only portably rely on an 8-bit field, s6_addr.

s6_addr_offset :: Int
s6_addr_offset = (#offset struct in6_addr, s6_addr)

peek32 :: Ptr a -> Int -> IO Word32
peek32 p i0 = do
    let i' = i0 * 4
        peekByte n = peekByteOff p (s6_addr_offset + i' + n) :: IO Word8
        a `sl` i = fromIntegral a `shiftL` i
    a0 <- peekByte 0
    a1 <- peekByte 1
    a2 <- peekByte 2
    a3 <- peekByte 3
    return ((a0 `sl` 24) .|. (a1 `sl` 16) .|. (a2 `sl` 8) .|. (a3 `sl` 0))

poke32 :: Ptr a -> Int -> Word32 -> IO ()
poke32 p i0 a = do
    let i' = i0 * 4
        pokeByte n = pokeByteOff p (s6_addr_offset + i' + n)
        x `sr` i = fromIntegral (x `shiftR` i) :: Word8
    pokeByte 0 (a `sr` 24)
    pokeByte 1 (a `sr` 16)
    pokeByte 2 (a `sr`  8)
    pokeByte 3 (a `sr`  0)

--------------------------------------------------------------------------------

-- Port Numbers

-- | Use the @Num@ instance (i.e. use a literal or 'fromIntegral') to create a
-- @PortNumber@ value with the correct network-byte-ordering.
--
-- >>> 1 :: PortNumber
-- 1
-- >>> read "1" :: PortNumber
-- 1
newtype PortNumber = PortNum Word16 deriving (Eq, Ord, Typeable)
-- newtyped to prevent accidental use of sane-looking
-- port numbers that haven't actually been converted to
-- network-byte-order first.

aNY_PORT :: PortNumber
aNY_PORT = 0

instance Show PortNumber where
  showsPrec p pn = showsPrec p (portNumberToInt pn)

instance Read PortNumber where
  readsPrec n = map (\(x,y) -> (intToPortNumber x, y)) . readsPrec n

intToPortNumber :: Int -> PortNumber
intToPortNumber v = PortNum (htons (fromIntegral v))

portNumberToInt :: PortNumber -> Int
portNumberToInt (PortNum po) = fromIntegral (ntohs po)

foreign import #{CALLCONV} unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import #{CALLCONV} unsafe "htons" htons :: Word16 -> Word16
foreign import #{CALLCONV} unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import #{CALLCONV} unsafe "htonl" htonl :: Word32 -> Word32

instance Enum PortNumber where
    toEnum   = intToPortNumber
    fromEnum = portNumberToInt

instance Num PortNumber where
   fromInteger i = intToPortNumber (fromInteger i)
    -- for completeness.
   (+) x y   = intToPortNumber (portNumberToInt x + portNumberToInt y)
   (-) x y   = intToPortNumber (portNumberToInt x - portNumberToInt y)
   negate x  = intToPortNumber (-portNumberToInt x)
   (*) x y   = intToPortNumber (portNumberToInt x * portNumberToInt y)
   abs n     = intToPortNumber (abs (portNumberToInt n))
   signum n  = intToPortNumber (signum (portNumberToInt n))

instance Real PortNumber where
    toRational x = toInteger x % 1

instance Integral PortNumber where
    quotRem a b = let (c,d) = quotRem (portNumberToInt a) (portNumberToInt b) in
                  (intToPortNumber c, intToPortNumber d)
    toInteger a = toInteger (portNumberToInt a)

instance Storable PortNumber where
   sizeOf    _ = sizeOf    (undefined :: Word16)
   alignment _ = alignment (undefined :: Word16)
   poke p (PortNum po) = poke (castPtr p) po
   peek p = PortNum `fmap` peek (castPtr p)
    
--------------------------------------------------------------------------------

newtype SocketFamily = SocketFamily CInt deriving (Show, Read, Eq, Ord, Typeable)
newtype SocketType = SocketType CInt deriving (Show, Read, Eq, Ord, Typeable)
newtype SocketProtocol = SocketProtocol CInt deriving (Show, Read, Eq, Ord, Typeable)

instance Storable SocketFamily where                      
    sizeOf _ = sizeOf (undefined :: CInt)       
    alignment _ = alignment (undefined :: CInt) 
    peek ptr = SocketFamily `fmap` peek (castPtr ptr)             
    poke ptr (SocketFamily v) = poke (castPtr ptr) v

instance Storable SocketType where                      
    sizeOf _ = sizeOf (undefined :: CInt)       
    alignment _ = alignment (undefined :: CInt) 
    peek ptr = SocketType `fmap` peek (castPtr ptr)             
    poke ptr (SocketType v) = poke (castPtr ptr) v

instance Storable SocketProtocol where                      
    sizeOf _ = sizeOf (undefined :: CInt)       
    alignment _ = alignment (undefined :: CInt) 
    peek ptr = SocketProtocol `fmap` peek (castPtr ptr)             
    poke ptr (SocketProtocol v) = poke (castPtr ptr) v

-- | unspecified
aF_UNSPEC           = SocketFamily (#const AF_UNSPEC          )
-- | internetwork: UDP, TCP, etc
aF_INET             = SocketFamily (#const AF_INET            )
-- | Internet Protocol version 6
aF_INET6            = SocketFamily (#const AF_INET6           )


sOCK_STREAM = SocketType (#const SOCK_STREAM)
sOCK_DGRAM = SocketType (#const SOCK_DGRAM) 
sOCK_RAW = SocketType (#const SOCK_RAW) 
sOCK_RDM = SocketType (#const SOCK_RDM) 
sOCK_SEQPACKET = SocketType (#const SOCK_SEQPACKET) 

iPPROTO_DEF :: SocketProtocol
iPPROTO_DEF = SocketProtocol 0

iPPROTO_IP :: SocketProtocol
iPPROTO_IP = SocketProtocol (#const IPPROTO_IP)

iPPROTO_TCP :: SocketProtocol
iPPROTO_TCP = SocketProtocol (#const IPPROTO_TCP)

iPPROTO_UDP :: SocketProtocol
iPPROTO_UDP = SocketProtocol (#const IPPROTO_UDP)
