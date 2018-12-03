{-|
Module      : Std.Data.Text.UTF8Rewind
Description : Errno provided by libuv
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

INTERNAL MODULE, provides utf8rewind constants

-}

module Std.Data.Text.UTF8Rewind where

import Foreign.C.Types

#include "utf8rewind.h"

newtype Locale = Locale CSize deriving (Show, Eq, Ord)

#{enum Locale, Locale, defaultLocale    = UTF8_LOCALE_DEFAULT }

