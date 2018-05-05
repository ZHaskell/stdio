{-|
Module      : System.IO.UV.Exception
Description : Extensible IO exceptions
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable


-}

module System.IO.UV.Exception where

import Foreign.C.Types
import Foreign.C.String

#include "uv.h"
#include "hs_uv.h"

-- Exception related

uvStdError :: CInt -> IO String
uvStdError errno = peekCString =<< uv_strerror errno

foreign import ccall unsafe uv_strerror :: CInt -> IO CString

uvErrName :: CInt -> IO String
uvErrName errno = peekCString =<< uv_err_name errno

foreign import ccall unsafe uv_err_name :: CInt -> IO CString

-- | argument list too long
#{enum CInt, CInt, uV_E2BIG          = UV_E2BIG          }
-- | permission denied
#{enum CInt, CInt, uV_EACCES         = UV_EACCES         }
-- | address already in use
#{enum CInt, CInt, uV_EADDRINUSE     = UV_EADDRINUSE     }
-- | address not available
#{enum CInt, CInt, uV_EADDRNOTAVAIL  = UV_EADDRNOTAVAIL  }
-- | address family not supported
#{enum CInt, CInt, uV_EAFNOSUPPORT   = UV_EAFNOSUPPORT   }
-- | resource temporarily unavailable
#{enum CInt, CInt, uV_EAGAIN         = UV_EAGAIN         }
-- | address family not supported
#{enum CInt, CInt, uV_EAI_ADDRFAMILY = UV_EAI_ADDRFAMILY }
-- | temporary failure
#{enum CInt, CInt, uV_EAI_AGAIN      = UV_EAI_AGAIN      }
-- | bad ai_flags value
#{enum CInt, CInt, uV_EAI_BADFLAGS   = UV_EAI_BADFLAGS   }
-- | invalid value for hints
#{enum CInt, CInt, uV_EAI_BADHINTS   = UV_EAI_BADHINTS   }
-- | request canceled
#{enum CInt, CInt, uV_EAI_CANCELED   = UV_EAI_CANCELED   }
-- | permanent failure
#{enum CInt, CInt, uV_EAI_FAIL       = UV_EAI_FAIL       }
-- | ai_family not supported
#{enum CInt, CInt, uV_EAI_FAMILY     = UV_EAI_FAMILY     }
-- | out of memory
#{enum CInt, CInt, uV_EAI_MEMORY     = UV_EAI_MEMORY     }
-- | no address
#{enum CInt, CInt, uV_EAI_NODATA     = UV_EAI_NODATA     }
-- | unknown node or service
#{enum CInt, CInt, uV_EAI_NONAME     = UV_EAI_NONAME     }
-- | argument buffer overflow
#{enum CInt, CInt, uV_EAI_OVERFLOW   = UV_EAI_OVERFLOW   }
-- | resolved protocol is unknown
#{enum CInt, CInt, uV_EAI_PROTOCOL   = UV_EAI_PROTOCOL   }
-- | service not available for socket type
#{enum CInt, CInt, uV_EAI_SERVICE    = UV_EAI_SERVICE    }
-- | socket type not supported
#{enum CInt, CInt, uV_EAI_SOCKTYPE   = UV_EAI_SOCKTYPE   }
-- | connection already in progress
#{enum CInt, CInt, uV_EALREADY       = UV_EALREADY       }
-- | bad file descriptor
#{enum CInt, CInt, uV_EBADF          = UV_EBADF          }
-- | resource busy or locked
#{enum CInt, CInt, uV_EBUSY          = UV_EBUSY          }
-- | operation canceled
#{enum CInt, CInt, uV_ECANCELED      = UV_ECANCELED      }
-- | invalid Unicode character
#{enum CInt, CInt, uV_ECHARSET       = UV_ECHARSET       }
-- | software caused connection abort
#{enum CInt, CInt, uV_ECONNABORTED   = UV_ECONNABORTED   }
-- | connection refused
#{enum CInt, CInt, uV_ECONNREFUSED   = UV_ECONNREFUSED   }
-- | connection reset by peer
#{enum CInt, CInt, uV_ECONNRESET     = UV_ECONNRESET     }
-- | destination address required
#{enum CInt, CInt, uV_EDESTADDRREQ   = UV_EDESTADDRREQ   }
-- | file already exists
#{enum CInt, CInt, uV_EEXIST         = UV_EEXIST         }
-- | bad address in system call argument
#{enum CInt, CInt, uV_EFAULT         = UV_EFAULT         }
-- | file too large
#{enum CInt, CInt, uV_EFBIG          = UV_EFBIG          }
-- | host is unreachable
#{enum CInt, CInt, uV_EHOSTUNREACH   = UV_EHOSTUNREACH   }
-- | interrupted system call
#{enum CInt, CInt, uV_EINTR          = UV_EINTR          }
-- | invalid argument
#{enum CInt, CInt, uV_EINVAL         = UV_EINVAL         }
-- | i/o error
#{enum CInt, CInt, uV_EIO            = UV_EIO            }
-- | socket is already connected
#{enum CInt, CInt, uV_EISCONN        = UV_EISCONN        }
-- | illegal operation on a directory
#{enum CInt, CInt, uV_EISDIR         = UV_EISDIR         }
-- | too many symbolic links encountered
#{enum CInt, CInt, uV_ELOOP          = UV_ELOOP          }
-- | too many open files
#{enum CInt, CInt, uV_EMFILE         = UV_EMFILE         }
-- | message too long
#{enum CInt, CInt, uV_EMSGSIZE       = UV_EMSGSIZE       }
-- | name too long
#{enum CInt, CInt, uV_ENAMETOOLONG   = UV_ENAMETOOLONG   }
-- | network is down
#{enum CInt, CInt, uV_ENETDOWN       = UV_ENETDOWN       }
-- | network is unreachable
#{enum CInt, CInt, uV_ENETUNREACH    = UV_ENETUNREACH    }
-- | file table overflow
#{enum CInt, CInt, uV_ENFILE         = UV_ENFILE         }
-- | no buffer space available
#{enum CInt, CInt, uV_ENOBUFS        = UV_ENOBUFS        }
-- | no such device
#{enum CInt, CInt, uV_ENODEV         = UV_ENODEV         }
-- | no such file or directory
#{enum CInt, CInt, uV_ENOENT         = UV_ENOENT         }
-- | not enough memory
#{enum CInt, CInt, uV_ENOMEM         = UV_ENOMEM         }
-- | machine is not on the network
#{enum CInt, CInt, uV_ENONET         = UV_ENONET         }
-- | protocol not available
#{enum CInt, CInt, uV_ENOPROTOOPT    = UV_ENOPROTOOPT    }
-- | no space left on device
#{enum CInt, CInt, uV_ENOSPC         = UV_ENOSPC         }
-- | function not implemented
#{enum CInt, CInt, uV_ENOSYS         = UV_ENOSYS         }
-- | socket is not connected
#{enum CInt, CInt, uV_ENOTCONN       = UV_ENOTCONN       }
-- | not a directory
#{enum CInt, CInt, uV_ENOTDIR        = UV_ENOTDIR        }
-- | directory not empty
#{enum CInt, CInt, uV_ENOTEMPTY      = UV_ENOTEMPTY      }
-- | socket operation on non-socket
#{enum CInt, CInt, uV_ENOTSOCK       = UV_ENOTSOCK       }
-- | operation not supported on socket
#{enum CInt, CInt, uV_ENOTSUP        = UV_ENOTSUP        }
-- | operation not permitted
#{enum CInt, CInt, uV_EPERM          = UV_EPERM          }
-- | broken pipe
#{enum CInt, CInt, uV_EPIPE          = UV_EPIPE          }
-- | protocol error
#{enum CInt, CInt, uV_EPROTO         = UV_EPROTO         }
-- | protocol not supported
#{enum CInt, CInt, uV_EPROTONOSUPPORT= UV_EPROTONOSUPPORT}
-- | protocol wrong type for socket
#{enum CInt, CInt, uV_EPROTOTYPE     = UV_EPROTOTYPE     }
-- | result too large
#{enum CInt, CInt, uV_ERANGE         = UV_ERANGE         }
-- | read-only file system
#{enum CInt, CInt, uV_EROFS          = UV_EROFS          }
-- | cannot send after transport endpoint shutdown
#{enum CInt, CInt, uV_ESHUTDOWN      = UV_ESHUTDOWN      }
-- | invalid seek
#{enum CInt, CInt, uV_ESPIPE         = UV_ESPIPE         }
-- | no such process
#{enum CInt, CInt, uV_ESRCH          = UV_ESRCH          }
-- | connection timed out
#{enum CInt, CInt, uV_ETIMEDOUT      = UV_ETIMEDOUT      }
-- | text file is busy
#{enum CInt, CInt, uV_ETXTBSY        = UV_ETXTBSY        }
-- | cross-device link not permitted
#{enum CInt, CInt, uV_EXDEV          = UV_EXDEV          }
-- | unknown error
#{enum CInt, CInt, uV_UNKNOWN        = UV_UNKNOWN        }
-- | end of file
#{enum CInt, CInt, uV_EOF            = UV_EOF            }
-- | no such device or address
#{enum CInt, CInt, uV_ENXIO          = UV_ENXIO          }
-- | too many links
#{enum CInt, CInt, uV_EMLINK         = UV_EMLINK         }
