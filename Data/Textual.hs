module Data.Builder.Textual where

class Textual a where
    textual :: a -> Builder

class Textual a => FormatTexutal a where
    data Format a
    format :: Format a -> a -> Builder



--------------------------------------------------------------------------------

class ParseTextual a where
    parseTextual :: Parser a
