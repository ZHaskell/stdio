{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : System.IO.UV.Exception
Description : Extensible IO exceptions
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module implemented extensible io exception following approach described in /An Extensible Dynamically-Typed
Hierarchy of Exceptions/ by Simon Marlow. The implementation in this module has simplified to meet common need.
User who want to catch certain type of exceptions can directly use exception types this module provide,
which are modeled after @IOErrorType@ from "GHC.IO.Exception".


This module also implements Gabriel Gonzalez'd idea on 'Resource' applicative:
<http://www.haskellforall.com/2013/06/the-resource-applicative.html>. The 'Applicative' and 'Monad' instance is
especially useful when you want safely combine multiple resources.

Functions from this package will throw exceptions from this module only instead of the old 'IOError' on I/O exceptions.
Exceptions from this module contain 'IOEInfo' which is pretty detailed, but this also require user of this module
do some extra work to keep error message's quality(provide CallStack, device informations, etc.).
New defined I/O exceptions are encouraged to include a 'IOEInfo', since it helps a lot when debugging.

Example for library author defining new io exception:

@
  data MyNetworkException = MyNetworkException IOEInfo ... deriving (Show, Typeable)
  instance Exception MyNetworkException where
        toException = ioExceptionToException
        fromException = ioExceptionFromException
@

If you're dealing with OS's errno directly, you should convert the errno to libuv's errno in C side with
'uv_translate_sys_error', then use 'throwUVIfMinus/throwUVError' from this module.

-}

module System.IO.Exception
  ( -- * The 'SomeIOException' type
    SomeIOException(..)
  , ioExceptionToException
  , ioExceptionFromException
    -- * Builtin io exception types
  , IOEInfo(..)
  , AlreadyExists(..)
  , NoSuchThing(..)
  , ResourceBusy(..)
  , ResourceExhausted(..)
  , EOF(..)
  , IllegalOperation(..)
  , PermissionDenied(..)
  , UnsatisfiedConstraints(..)
  , SystemError(..)
  , ProtocolError(..)
  , OtherError(..)
  , InvalidArgument(..)
  , InappropriateType(..)
  , HardwareFault(..)
  , UnsupportedOperation(..)
  , TimeExpired(..)
  , ResourceVanished(..)
  , Interrupted(..)
    -- * Throw io exceptions
  , throwOOMIfNull
  , throwUVIfMinus
  , throwUVIfMinus_
    -- * Resource management
  , Resource(..)
  , initResource
  , initResource_
  , withResource
  , withResource'
    -- * Re-exports
  , module Control.Exception
  , HasCallStack
  , callStack
  ) where

import Control.Exception hiding (IOException)
import Control.Monad
import Control.Monad.IO.Class
import Data.Typeable
import Data.IORef.Unboxed
import Foreign.Ptr
import Foreign.C.Types
import GHC.Stack.Compat
import System.IO.UV.Exception

-- | The root type of all io exceptions, you can catch all io exception by catching this root type.
--
data SomeIOException = forall e . Exception e => SomeIOException e
    deriving Typeable

instance Show SomeIOException where
    show (SomeIOException e) = show e

instance Exception SomeIOException

ioExceptionToException :: Exception e => e -> SomeException
ioExceptionToException = toException . SomeIOException

ioExceptionFromException :: Exception e => SomeException -> Maybe e
ioExceptionFromException x = do
    SomeIOException a <- fromException x
    cast a

#define IOE(e) data e = e IOEInfo deriving (Show, Typeable);  \
               instance Exception e where                     \
                   { toException = ioExceptionToException     \
                   ; fromException = ioExceptionFromException \
                   }
IOE(AlreadyExists)
IOE(NoSuchThing)
IOE(ResourceBusy)
IOE(ResourceExhausted)
IOE(EOF)
IOE(IllegalOperation)
IOE(PermissionDenied)
IOE(UnsatisfiedConstraints)
IOE(SystemError)
IOE(ProtocolError)
IOE(OtherError)
IOE(InvalidArgument)
IOE(InappropriateType)
IOE(HardwareFault)
IOE(UnsupportedOperation)
IOE(TimeExpired)
IOE(ResourceVanished)
IOE(Interrupted)

--------------------------------------------------------------------------------

-- | Throw 'ResourceExhausted' if allocation return a 'nullPtr'.
--
throwOOMIfNull :: HasCallStack
               => IO (Ptr a)    -- ^ the allocation action
               -> IO (Ptr a)
throwOOMIfNull f = do
    addr <- f
    if addr == nullPtr
        then throwIO (ResourceExhausted (IOEInfo "OOM" "out of memory when doing allocation" callStack))
        else return addr

-- | Throw appropriate IO exception if return value < 0 (libuv's convention).
--
throwUVIfMinus :: (HasCallStack, Integral a)
               => IO a    -- ^ the IO action
               -> IO a
throwUVIfMinus f = do
    errno <- f
    let errno' = fromIntegral errno
    if errno' < 0
        then do
            name <- uvErrName errno'
            desc <- uvStdError errno'
            throwUVError errno' (IOEInfo name desc callStack)
        else return errno

-- | Throw appropriate IO exception if return value < 0, otherwise ignore the result.
--
throwUVIfMinus_ :: (HasCallStack, Integral a)
                => IO a    -- ^ the IO action
                -> IO ()
throwUVIfMinus_ f = do
    errno <- f
    let errno' = fromIntegral errno
    when (errno' < 0) $ do
        name <- uvErrName errno'
        desc <- uvStdError errno'
        throwUVError errno' (IOEInfo name desc callStack)

--------------------------------------------------------------------------------

-- | IO exceptions informations.
--
data IOEInfo = IOEInfo
    { ioeName        :: String      -- ^ the errno name, e.g. EADDRINUSE, etc. empty if no errno.
    , ioeDescription :: String      -- ^ description for this io error, can be errno description, or some custom description if no errno.
    , ioeCallStack   :: CallStack   -- ^ lightweight partial call-stack
    }

instance Show IOEInfo where
    show (IOEInfo errno desc cstack) =
         "{name:" ++ errno ++
         ", description:" ++ desc ++
         ", callstack:" ++ prettyCallStack cstack ++ "}"

throwUVError :: CInt -> IOEInfo -> IO a
throwUVError e info
    | e == uV_EOF             = throwIO (EOF                     info)
    | e == uV_E2BIG           = throwIO (ResourceExhausted       info)
    | e == uV_EACCES          = throwIO (PermissionDenied        info)
    | e == uV_EADDRINUSE      = throwIO (ResourceBusy            info)
    | e == uV_EADDRNOTAVAIL   = throwIO (UnsupportedOperation    info)
    | e == uV_EAFNOSUPPORT    = throwIO (UnsupportedOperation    info)
    | e == uV_EAGAIN          = throwIO (ResourceExhausted       info)
    | e == uV_EAI_ADDRFAMILY  = throwIO (UnsupportedOperation    info)
    | e == uV_EAI_AGAIN       = throwIO (ResourceExhausted       info)
    | e == uV_EAI_BADFLAGS    = throwIO (UnsupportedOperation    info)
    | e == uV_EAI_BADHINTS    = throwIO (UnsupportedOperation    info)
    | e == uV_EAI_CANCELED    = throwIO (ResourceVanished        info)
    | e == uV_EAI_FAIL        = throwIO (OtherError              info)
    | e == uV_EAI_FAMILY      = throwIO (UnsupportedOperation    info)
    | e == uV_EAI_MEMORY      = throwIO (ResourceExhausted       info)
    | e == uV_EAI_NODATA      = throwIO (NoSuchThing             info)
    | e == uV_EAI_NONAME      = throwIO (NoSuchThing             info)
    | e == uV_EAI_OVERFLOW    = throwIO (InvalidArgument         info)
    | e == uV_EAI_PROTOCOL    = throwIO (ProtocolError           info)
    | e == uV_EAI_SERVICE     = throwIO (UnsupportedOperation    info)
    | e == uV_EAI_SOCKTYPE    = throwIO (UnsupportedOperation    info)
    | e == uV_EALREADY        = throwIO (AlreadyExists           info)
    | e == uV_EBADF           = throwIO (InvalidArgument         info)
    | e == uV_EBUSY           = throwIO (ResourceBusy            info)
    | e == uV_ECANCELED       = throwIO (ResourceVanished        info)
    | e == uV_ECHARSET        = throwIO (OtherError              info)
    | e == uV_ECONNABORTED    = throwIO (ResourceVanished        info)
    | e == uV_ECONNREFUSED    = throwIO (NoSuchThing             info)
    | e == uV_ECONNRESET      = throwIO (ResourceVanished        info)
    | e == uV_EDESTADDRREQ    = throwIO (InvalidArgument         info)
    | e == uV_EEXIST          = throwIO (AlreadyExists           info)
    | e == uV_EFAULT          = throwIO (OtherError              info)
    | e == uV_EFBIG           = throwIO (PermissionDenied        info)
    | e == uV_EHOSTUNREACH    = throwIO (NoSuchThing             info)
    | e == uV_EINTR           = throwIO (Interrupted             info)
    | e == uV_EINVAL          = throwIO (InvalidArgument         info)
    | e == uV_EIO             = throwIO (HardwareFault           info)
    | e == uV_EISCONN         = throwIO (AlreadyExists           info)
    | e == uV_EISDIR          = throwIO (InappropriateType       info)
    | e == uV_ELOOP           = throwIO (InvalidArgument         info)
    | e == uV_EMFILE          = throwIO (ResourceExhausted       info)
    | e == uV_EMSGSIZE        = throwIO (ResourceExhausted       info)
    | e == uV_ENAMETOOLONG    = throwIO (InvalidArgument         info)
    | e == uV_ENETDOWN        = throwIO (ResourceVanished        info)
    | e == uV_ENETUNREACH     = throwIO (NoSuchThing             info)
    | e == uV_ENFILE          = throwIO (ResourceExhausted       info)
    | e == uV_ENOBUFS         = throwIO (ResourceExhausted       info)
    | e == uV_ENODEV          = throwIO (UnsupportedOperation    info)
    | e == uV_ENOENT          = throwIO (NoSuchThing             info)
    | e == uV_ENOMEM          = throwIO (ResourceExhausted       info)
    | e == uV_ENOPROTOOPT     = throwIO (UnsupportedOperation    info)
    | e == uV_ENOSPC          = throwIO (ResourceExhausted       info)
    | e == uV_ENOSYS          = throwIO (UnsupportedOperation    info)
    | e == uV_ENOTCONN        = throwIO (InvalidArgument         info)
    | e == uV_ENOTDIR         = throwIO (InappropriateType       info)
    | e == uV_ENOTEMPTY       = throwIO (UnsatisfiedConstraints  info)
    | e == uV_ENOTSOCK        = throwIO (InvalidArgument         info)
    | e == uV_ENOTSUP         = throwIO (UnsupportedOperation    info)
    | e == uV_EPERM           = throwIO (PermissionDenied        info)
    | e == uV_EPIPE           = throwIO (ResourceVanished        info)
    | e == uV_EPROTO          = throwIO (ProtocolError           info)
    | e == uV_EPROTONOSUPPORT = throwIO (ProtocolError           info)
    | e == uV_EPROTOTYPE      = throwIO (ProtocolError           info)
    | e == uV_ERANGE          = throwIO (UnsupportedOperation    info)
    | e == uV_EROFS           = throwIO (PermissionDenied        info)
    | e == uV_ESHUTDOWN       = throwIO (IllegalOperation        info)
    | e == uV_ESPIPE          = throwIO (UnsupportedOperation    info)
    | e == uV_ESRCH           = throwIO (NoSuchThing             info)
    | e == uV_ETIMEDOUT       = throwIO (TimeExpired             info)
    | e == uV_ETXTBSY         = throwIO (ResourceBusy            info)
    | e == uV_EXDEV           = throwIO (UnsupportedOperation    info)
    | e == uV_UNKNOWN         = throwIO (OtherError              info)
    | e == uV_ENXIO           = throwIO (NoSuchThing             info)
    | e == uV_EMLINK          = throwIO (ResourceExhausted       info)
    | otherwise               = throwIO (OtherError              info)

--------------------------------------------------------------------------------

-- | A 'Resource' is an 'IO' action which acquires some resource of type a and
-- also returns a finalizer of type IO () that releases the resource.
--
-- The only safe way to use a 'Resource' is 'with' \/ 'with\'', You should NEVER
-- use the `acquire` field directly, unless you want to give up resource management.
--
-- 'MonadIO' instance is provided so that you can lift 'IO' computation inside
-- 'Resource', this is convenient for propagate 'Resource' around since many
-- 'IO' computations carry finalizers.
--
-- A convention in stdio is that functions returning a 'Resource' should be
-- named in @initXXX@ format, users are strongly recommended to follow this convention.
--
newtype Resource a = Resource { acquire :: IO (a, IO ()) }

-- | Create 'Resource' from create and release action.
--
-- Note, 'resource' doesn't open resource itself, resource is created when you use
-- 'with' \/ 'with''.
--
initResource :: IO a -> (a -> IO ()) -> Resource a
{-# INLINE initResource #-}
initResource create release = Resource $ do
    r <- create
    return $ (r, release r)

-- | Create 'Resource' from create and release action.
--
-- This function is useful when you want to add some initialization and clean up action
-- inside 'Resource' monad.
--
initResource_ :: IO () -> IO () -> Resource ()
{-# INLINE initResource_ #-}
initResource_ create release = Resource $ do
    r <- create
    return $ (r, release)

instance Functor Resource where
    {-# INLINE fmap #-}
    fmap f resource = Resource $ do
        (a, release) <- acquire resource
        return (f a, release)

instance Applicative Resource where
    {-# INLINE pure #-}
    pure a = Resource (pure (a, pure ()))
    {-# INLINE (<*>) #-}
    resource1 <*> resource2 = Resource $ do
        (f, release1) <- acquire resource1
        (x, release2) <- acquire resource2 `onException` release1
        return (f x, release2 >> release1)

instance Monad Resource where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    m >>= f = Resource $ do
        (m', release1) <- acquire m
        (x , release2) <- acquire (f m') `onException` release1
        return (x, release2 >> release1)

instance MonadIO Resource where
    {-# INLINE liftIO #-}
    liftIO f = Resource $ fmap (\ a -> (a, dummyRelease)) f
        where dummyRelease = return ()

-- | Create a new resource and run some computation, resource is guarantee to
-- be closed.
--
-- Be care don't leak the resource through computation return value, because
-- after the computation finishes, the resource is closed already.
--
withResource :: Resource a -> (a -> IO b) -> IO b
withResource resource k = bracket (acquire resource)
                                 (\(_, release) -> release)
                                 (\(a, _) -> k a)

-- | Create a new resource and run some computation, resource is guarantee to
-- be closed.
--
-- The difference from 'with' is that the computation will receive an extra
-- close action, which can be used to close the resource early before the whole
-- computation finished, the close action can be called multiple times,
-- only the first call will clean up the resource.
--
withResource' :: Resource a -> (a -> IO () -> IO b) -> IO b
withResource' resource k = do
    c <- newCounter 0
    bracket (do (a, release) <- acquire resource
                let release' = do
                        c' <- atomicOrCounter_ c 1
                        when (c' == 0) release
                return (a, release'))
             (\(_, release) -> release)
             (\(a, release) -> k a release)
