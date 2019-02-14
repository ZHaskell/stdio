{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Std.Data.Parser
Description : Efficient deserialization/parse.
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide a simple resumable 'Parser', which is suitable for binary protocol and simple textual protocol parsing.

You can use 'Alternative' instance to do backtracking, each branch will either succeed and may consume some input, or fail without consume anything. It's recommend to use 'peek' to avoid backtracking if possible to get high performance.

-}
module Std.Data.Parser
  ( -- * Parser types
    Result
  , Parser
    -- * Running a parser
  , parse, parse', parseChunk, parseChunks, finishParsing
  , runAndKeepTrack
    -- * Basic parsers
  , ensureN, endOfInput
    -- * Primitive decoders
  , decodePrim, decodePrimLE, decodePrimBE
    -- * More parsers
  , scan, scanChunks, peekMaybe, peek, satisfy, satisfyWith
  , word8, anyWord8, endOfLine, skip, skipWhile, skipSpaces
  , take, takeTill, takeWhile, takeWhile1, bytes, bytesCI
  , text
    -- * Numeric parsers
    -- ** Decimal
  , uint, int
    -- ** Hex
  , hex
    -- ** Fractional
  , rational
  , float, double
  , scientific
  , scientifically
  ) where

import           Std.Data.Parser.Base
import           Std.Data.Parser.Numeric
import           Prelude hiding (take, takeWhile)
