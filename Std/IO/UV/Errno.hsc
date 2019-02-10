{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Std.IO.UVErrno
Description : Errno provided by libuv
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

INTERNAL MODULE, provides all libuv errno.

-}

module Std.IO.UV.Errno where

import Foreign.C.Types
import Foreign.C.String

#include "hs_uv.h"

uvStdError :: CInt -> IO String
uvStdError errno = peekCString =<< uv_strerror errno

foreign import ccall unsafe uv_strerror :: CInt -> IO CString

uvErrName :: CInt -> IO String
uvErrName errno = peekCString =<< uv_err_name errno

foreign import ccall unsafe uv_err_name :: CInt -> IO CString

-- | argument list too long
pattern UV_E2BIG           :: CInt
pattern UV_E2BIG           = #{const UV_E2BIG          }
-- | permission denied
pattern UV_EACCES          :: CInt
pattern UV_EACCES          = #{const UV_EACCES         }
-- | address already in use
pattern UV_EADDRINUSE      :: CInt
pattern UV_EADDRINUSE      = #{const UV_EADDRINUSE     }
-- | address not available
pattern UV_EADDRNOTAVAIL   :: CInt
pattern UV_EADDRNOTAVAIL   = #{const UV_EADDRNOTAVAIL  }
-- | address family not supported
pattern UV_EAFNOSUPPORT    :: CInt
pattern UV_EAFNOSUPPORT    = #{const UV_EAFNOSUPPORT   }
-- | resource temporarily unavailable
pattern UV_EAGAIN          :: CInt
pattern UV_EAGAIN          = #{const UV_EAGAIN         }
-- | address family not supported
pattern UV_EAI_ADDRFAMILY  :: CInt
pattern UV_EAI_ADDRFAMILY  = #{const UV_EAI_ADDRFAMILY }
-- | temporary failure
pattern UV_EAI_AGAIN       :: CInt
pattern UV_EAI_AGAIN       = #{const UV_EAI_AGAIN      }
-- | bad ai_flags value
pattern UV_EAI_BADFLAGS    :: CInt
pattern UV_EAI_BADFLAGS    = #{const UV_EAI_BADFLAGS   }
-- | invalid value for hints
pattern UV_EAI_BADHINTS    :: CInt
pattern UV_EAI_BADHINTS    = #{const UV_EAI_BADHINTS   }
-- | request canceled
pattern UV_EAI_CANCELED    :: CInt
pattern UV_EAI_CANCELED    = #{const UV_EAI_CANCELED   }
-- | permanent failure
pattern UV_EAI_FAIL        :: CInt
pattern UV_EAI_FAIL        = #{const UV_EAI_FAIL       }
-- | ai_family not supported
pattern UV_EAI_FAMILY      :: CInt
pattern UV_EAI_FAMILY      = #{const UV_EAI_FAMILY     }
-- | out of memory
pattern UV_EAI_MEMORY      :: CInt
pattern UV_EAI_MEMORY      = #{const UV_EAI_MEMORY     }
-- | no address
pattern UV_EAI_NODATA      :: CInt
pattern UV_EAI_NODATA      = #{const UV_EAI_NODATA     }
-- | unknown node or service
pattern UV_EAI_NONAME      :: CInt
pattern UV_EAI_NONAME      = #{const UV_EAI_NONAME     }
-- | argument buffer overflow
pattern UV_EAI_OVERFLOW    :: CInt
pattern UV_EAI_OVERFLOW    = #{const UV_EAI_OVERFLOW   }
-- | resolved protocol is unknown
pattern UV_EAI_PROTOCOL    :: CInt
pattern UV_EAI_PROTOCOL    = #{const UV_EAI_PROTOCOL   }
-- | service not available for socket type
pattern UV_EAI_SERVICE     :: CInt
pattern UV_EAI_SERVICE     = #{const UV_EAI_SERVICE    }
-- | socket type not supported
pattern UV_EAI_SOCKTYPE    :: CInt
pattern UV_EAI_SOCKTYPE    = #{const UV_EAI_SOCKTYPE   }
-- | connection already in progress
pattern UV_EALREADY        :: CInt
pattern UV_EALREADY        = #{const UV_EALREADY       }
-- | bad file descriptor
pattern UV_EBADF           :: CInt
pattern UV_EBADF           = #{const UV_EBADF          }
-- | resource busy or locked
pattern UV_EBUSY           :: CInt
pattern UV_EBUSY           = #{const UV_EBUSY          }
-- | operation canceled
pattern UV_ECANCELED       :: CInt
pattern UV_ECANCELED       = #{const UV_ECANCELED      }
-- | invalid Unicode character
pattern UV_ECHARSET        :: CInt
pattern UV_ECHARSET        = #{const UV_ECHARSET       }
-- | software caused connection abort
pattern UV_ECONNABORTED    :: CInt
pattern UV_ECONNABORTED    = #{const UV_ECONNABORTED   }
-- | connection refused
pattern UV_ECONNREFUSED    :: CInt
pattern UV_ECONNREFUSED    = #{const UV_ECONNREFUSED   }
-- | connection reset by peer
pattern UV_ECONNRESET      :: CInt
pattern UV_ECONNRESET      = #{const UV_ECONNRESET     }
-- | destination address required
pattern UV_EDESTADDRREQ    :: CInt
pattern UV_EDESTADDRREQ    = #{const UV_EDESTADDRREQ   }
-- | file already exists
pattern UV_EEXIST          :: CInt
pattern UV_EEXIST          = #{const UV_EEXIST         }
-- | bad address in system call argument
pattern UV_EFAULT          :: CInt
pattern UV_EFAULT          = #{const UV_EFAULT         }
-- | file too large
pattern UV_EFBIG           :: CInt
pattern UV_EFBIG           = #{const UV_EFBIG          }
-- | host is unreachable
pattern UV_EHOSTUNREACH    :: CInt
pattern UV_EHOSTUNREACH    = #{const UV_EHOSTUNREACH   }
-- | interrupted system call
pattern UV_EINTR           :: CInt
pattern UV_EINTR           = #{const UV_EINTR          }
-- | invalid argument
pattern UV_EINVAL          :: CInt
pattern UV_EINVAL          = #{const UV_EINVAL         }
-- | i/o error
pattern UV_EIO             :: CInt
pattern UV_EIO             = #{const UV_EIO            }
-- | socket is already connected
pattern UV_EISCONN         :: CInt
pattern UV_EISCONN         = #{const UV_EISCONN        }
-- | illegal operation on a directory
pattern UV_EISDIR          :: CInt
pattern UV_EISDIR          = #{const UV_EISDIR         }
-- | too many symbolic links encountered
pattern UV_ELOOP           :: CInt
pattern UV_ELOOP           = #{const UV_ELOOP          }
-- | too many open files
pattern UV_EMFILE          :: CInt
pattern UV_EMFILE          = #{const UV_EMFILE         }
-- | message too long
pattern UV_EMSGSIZE        :: CInt
pattern UV_EMSGSIZE        = #{const UV_EMSGSIZE       }
-- | name too long
pattern UV_ENAMETOOLONG    :: CInt
pattern UV_ENAMETOOLONG    = #{const UV_ENAMETOOLONG   }
-- | network is down
pattern UV_ENETDOWN        :: CInt
pattern UV_ENETDOWN        = #{const UV_ENETDOWN       }
-- | network is unreachable
pattern UV_ENETUNREACH     :: CInt
pattern UV_ENETUNREACH     = #{const UV_ENETUNREACH    }
-- | file table overflow
pattern UV_ENFILE          :: CInt
pattern UV_ENFILE          = #{const UV_ENFILE         }
-- | no buffer space available
pattern UV_ENOBUFS         :: CInt
pattern UV_ENOBUFS         = #{const UV_ENOBUFS        }
-- | no such device
pattern UV_ENODEV          :: CInt
pattern UV_ENODEV          = #{const UV_ENODEV         }
-- | no such file or directory
pattern UV_ENOENT          :: CInt
pattern UV_ENOENT          = #{const UV_ENOENT         }
-- | not enough memory
pattern UV_ENOMEM          :: CInt
pattern UV_ENOMEM          = #{const UV_ENOMEM         }
-- | machine is not on the network
pattern UV_ENONET          :: CInt
pattern UV_ENONET          = #{const UV_ENONET         }
-- | protocol not available
pattern UV_ENOPROTOOPT     :: CInt
pattern UV_ENOPROTOOPT     = #{const UV_ENOPROTOOPT    }
-- | no space left on device
pattern UV_ENOSPC          :: CInt
pattern UV_ENOSPC          = #{const UV_ENOSPC         }
-- | function not implemented
pattern UV_ENOSYS          :: CInt
pattern UV_ENOSYS          = #{const UV_ENOSYS         }
-- | socket is not connected
pattern UV_ENOTCONN        :: CInt
pattern UV_ENOTCONN        = #{const UV_ENOTCONN       }
-- | not a directory
pattern UV_ENOTDIR         :: CInt
pattern UV_ENOTDIR         = #{const UV_ENOTDIR        }
-- | directory not empty
pattern UV_ENOTEMPTY       :: CInt
pattern UV_ENOTEMPTY       = #{const UV_ENOTEMPTY      }
-- | socket operation on non-socket
pattern UV_ENOTSOCK        :: CInt
pattern UV_ENOTSOCK        = #{const UV_ENOTSOCK       }
-- | operation not supported on socket
pattern UV_ENOTSUP         :: CInt
pattern UV_ENOTSUP         = #{const UV_ENOTSUP        }
-- | operation not permitted
pattern UV_EPERM           :: CInt
pattern UV_EPERM           = #{const UV_EPERM          }
-- | broken pipe
pattern UV_EPIPE           :: CInt
pattern UV_EPIPE           = #{const UV_EPIPE          }
-- | protocol error
pattern UV_EPROTO          :: CInt
pattern UV_EPROTO          = #{const UV_EPROTO         }
-- | protocol not supported
pattern UV_EPROTONOSUPPORT :: CInt
pattern UV_EPROTONOSUPPORT = #{const UV_EPROTONOSUPPORT}
-- | protocol wrong type for socket
pattern UV_EPROTOTYPE      :: CInt
pattern UV_EPROTOTYPE      = #{const UV_EPROTOTYPE     }
-- | result too large
pattern UV_ERANGE          :: CInt
pattern UV_ERANGE          = #{const UV_ERANGE         }
-- | read-only file system
pattern UV_EROFS           :: CInt
pattern UV_EROFS           = #{const UV_EROFS          }
-- | cannot send after transport endpoint shutdown
pattern UV_ESHUTDOWN       :: CInt
pattern UV_ESHUTDOWN       = #{const UV_ESHUTDOWN      }
-- | invalid seek
pattern UV_ESPIPE          :: CInt
pattern UV_ESPIPE          = #{const UV_ESPIPE         }
-- | no such process
pattern UV_ESRCH           :: CInt
pattern UV_ESRCH           = #{const UV_ESRCH          }
-- | connection timed out
pattern UV_ETIMEDOUT       :: CInt
pattern UV_ETIMEDOUT       = #{const UV_ETIMEDOUT      }
-- | text file is busy
pattern UV_ETXTBSY         :: CInt
pattern UV_ETXTBSY         = #{const UV_ETXTBSY        }
-- | cross-device link not permitted
pattern UV_EXDEV           :: CInt
pattern UV_EXDEV           = #{const UV_EXDEV          }
-- | unknown error
pattern UV_UNKNOWN         :: CInt
pattern UV_UNKNOWN         = #{const UV_UNKNOWN        }
-- | end of file
pattern UV_EOF             :: CInt
pattern UV_EOF             = #{const UV_EOF            }
-- | no such device or address
pattern UV_ENXIO           :: CInt
pattern UV_ENXIO           = #{const UV_ENXIO          }
-- | too many links
pattern UV_EMLINK          :: CInt
pattern UV_EMLINK          = #{const UV_EMLINK         }
