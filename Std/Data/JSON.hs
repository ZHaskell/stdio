{-|
Module      : Std.Data.JSON
Description : Fast JSON serialization/deserialization
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

-}

module Std.Data.JSON
  ( -- * Encode & Decode
    DecodeError
  , decode', decode, decodeChunks, decodeChunks', encodeBytes, encodeText, encodeTextBuilder
  -- * Convert 'Value' to Haskell data
  , convert, convert', Converter(..), fail', (<?>), prependContext
  , PathElement(..), ConvertError
  , typeMismatch, fromNull, withBool, withScientific, withBoundedScientific, withRealFloat
  , withBoundedIntegral, withText, withArray, withKeyValues, withFlatMap, withFlatMapR
  , withHashMap, withHashMapR, withEmbeddedJSON
  , (.:), (.:?), (.:!), convertField, convertFieldMaybe, convertFieldMaybe'
  -- * FromValue, ToValue & EncodeJSON
  , defaultSettings, Settings(..)
  , ToValue(..)
  , FromValue(..)
  , EncodeJSON(..)
  ) where


import Std.Data.JSON.Base
