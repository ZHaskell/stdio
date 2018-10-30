{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Std.Data.Text.UTF8Rewind where

import Std.Data.Vector.Base
import Std.Foreign.PrimArray
import Foreign.Ptr
import Foreign.C.Types
import Data.Int
import GHC.Prim

#include "utf8rewind.h"

uTF8_LOCALE_DEFAULT :: CSize
uTF8_LOCALE_DEFAULT = #const UTF8_LOCALE_DEFAULT


foreign import ccall unsafe utf8_validate :: Addr## -> Int## -> Int## -> Int##



