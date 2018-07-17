{-|
Module      : Std.Data.Textual
Description : Textual format/parse.
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

WIP

-}

module Std.Data.Textual where

class Format a where
    format :: a -> Builder ()

class Parse a where
    parse :: Parser a


--------------------------------------------------------------------------------

formatDec ::
formatHex ::
formatFP  ::

formatText :: Text ->
formatASCII :: Bytes ->
