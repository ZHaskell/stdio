{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

{-|
Module      : Std.Data.Builder
Description : Efficient serialization/format.
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

A 'Builder' records a buffer writing function, which can be 'mappend' in O(1) via composition. This module provides many functions to turn basic data types into 'Builder's, which can used to build strict 'Bytes' or list of 'Bytes' chunks.

-}

module Std.Data.Builder
  ( -- * Builder type
    Builder
  , append
   -- * Running builders
  , buildBytes
  , buildBytesWith
  , buildBytesList
  , buildBytesListWith
  , buildAndRun
  , buildAndRunWith
    -- * Basic buiders
  , bytes
  , ensureN
  , atMost
  , writeN
   -- * Pritimive builders
  , encodePrim
  , encodePrimLE
  , encodePrimBE
  -- * More builders
  , stringUTF8, charUTF8, string7, char7, string8, char8, text
  -- * Numeric builders
  -- ** Integral type formatting
  , IFormat(..)
  , defaultIFormat
  , Padding(..)
  , int
  , intWith
  , integer
  -- ** Fixded size hexidecimal formatting
  , hex, heX
  -- ** IEEE float formating
  , FFFormat(..)
  , double
  , doubleWith
  , float
  , floatWith
  , scientific
  , scientificWith
  ) where

import           Std.Data.Builder.Base
import           Std.Data.Builder.Numeric
