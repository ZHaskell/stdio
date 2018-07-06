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

class Textual a where
    textual :: a -> Builder

class Textual a => FormatTexutal a where
    data Format a
    format :: Format a -> a -> Builder



--------------------------------------------------------------------------------

class ParseTextual a where
    parseTextual :: Parser a
