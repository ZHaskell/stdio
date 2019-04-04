{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : Std.Data.JSON.Generic
Description : Fast JSON serialization/deserialization
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

-}

module Std.Data.JSON.Generic where

import qualified Data.HashMap.Strict     as HM
import qualified Std.Data.Vector.FlatMap as FM
import qualified Std.Data.Vector         as V
import qualified Std.Data.Text           as T
import qualified Control.Monad.Fail      as Fail
import           Data.Scientific         (Scientific)
import qualified Std.Data.Builder        as B
import qualified Std.Data.TextBuilder    as TB
import qualified Std.Data.Parser         as P
import           Std.Data.JSON.Value
import qualified Std.Data.JSON.Value.Builder as JB
import Data.Data
import Data.Typeable
import GHC.Generics

data Settings = Settings
    { fieldNameFmt :: String -> T.Text
    , conNameFmt :: String -> T.Text
    }

-- | Class for converting the functors from "GHC.Generics" to JSON.
-- You generally don't need to give any custom instances. Just add
-- 'deriving Generic' and call 'gToJson'.
class GtoJson f where
  -- | Generically show a functor as a JSON value.  The first argument
  -- tells us if there are multiple constructors in the data type. The
  -- second indicates if this data type is an enumeration (only empty
  -- constructors). A functor is then converted to either a list
  -- of values (for non-labeled fields) or a list of String/value
  -- pairs (for labeled fields).
  gtoJSONf :: Settings -> f a -> Value
  gbuildJSONf :: Settings -> f a -> TextBuilder ()

-- | Class for parsing the functors from "GHC.Generics" from JSON.
-- You generally don't need to give any custom instances. Just add
-- 'deriving Generic' and call 'gFromJson'.
class GfromJson f where
  -- | Generically read a functor from a JSON value.  The first
  -- argument tells us if there are multiple constructors in the data
  -- type. The second indicates if we've already detected that this
  -- data type has multiple constructors. When this is False, the
  -- (:*:) puts the fields in the state. The third indicates if this
  -- data type is an enumeration (only empty constructors). The third
  -- is a function for parsing the recursive positions. A JSON value
  -- is then parsed to either a functor, or a failure.
  gparseJSONf :: Settings -> Bool -> Bool -> Bool -> StateT [Value] Parser (f a)

-- Void: Used for data types without constructors
-- instance GJSON V1

-- Unit: Used for constructors without arguments
instance GtoJson U1 where
  gtoJSONf _ _ _ U1 = Right []
instance GfromJson U1 where
  gparseJSONf _ _ _ _ = return U1
