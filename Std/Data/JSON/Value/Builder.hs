{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UnliftedFFITypes   #-}

{-|
Module      : Std.Data.JSON.Value.Builder
Description : JSON representation and builders
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides builders for JSON 'Value's, a Haskell JSON representation. The builders are designed to comply with <https://tools.ietf.org/html/rfc8258 rfc8258>. Only control characters are escaped, other unicode codepoint are directly written instead of escaped.

-}
module Std.Data.JSON.Value.Builder
  ( -- * Value type
    Value(..)
    -- * Value Builders
  , value
  , object
  , array
  , string
  ) where

import           Control.Monad
import           Control.Monad.ST.Unsafe  (unsafeIOToST)
import           Data.Bits                (shiftL)
import           Data.Functor
import           Data.Primitive.PrimArray
import           Data.Scientific          (Scientific)
import           Data.Typeable
import           Data.Word
import           GHC.Prim                 (unsafeCoerce#)
import qualified Std.Data.Builder              as B
import qualified Std.Data.Builder.Base         as B
import qualified Std.Data.Text            as T
import qualified Std.Data.Text.Base       as T
import           Std.Data.Vector.Base     as V
import           Std.Data.Vector.Extra    as V
import           Std.Foreign.PrimArray
import           Std.Data.JSON.Value      (Value(..))

#define DOUBLE_QUOTE 34

value :: Value -> B.Builder ()
{-# INLINABLE value #-}
value (Object kvs) = object kvs
value (Array vs) = array vs
value (String t) = string t
value (Number n) = B.scientific n
value (Bool True) = "true"
value (Bool False) = "false"
value Null = "null"

array :: V.Vector Value -> B.Builder ()
array vs = do
    B.char8 '['
    forM_ (V.initMayEmpty vs) $ \ v-> value v >> B.char8 ','
    forM_ (V.lastMaybe vs) $ \ v-> value v
    B.char8 ']'

object :: V.Vector (T.Text, Value) -> B.Builder ()
object kvs = do
    B.char8 '{'
    forM_ (V.initMayEmpty kvs) $ \ (k, v) -> do
        string k
        B.char8 ':'
        value v
        B.char8 ','
    forM_ (V.lastMaybe kvs) $ \ (k, v) -> do
        string k
        B.char8 ':'
        value v
    B.char8 '}'

string :: T.Text -> B.Builder ()
string (T.Text (V.PrimVector ba@(PrimArray ba#) s l)) = do
    let siz = escape_json_string_length ba# s l
    B.ensureN siz
    B.Builder (\ _  k (B.Buffer mba@(MutablePrimArray mba#) i) -> do
        if siz == l+2
        then do
            writePrimArray mba i DOUBLE_QUOTE
            copyPrimArray mba (i+1) ba s l
            writePrimArray mba (i+1+l) DOUBLE_QUOTE
        else void $ unsafeIOToST (escape_json_string ba# s l (unsafeCoerce# mba#) i)
        k () (B.Buffer mba (i+siz)))

foreign import ccall unsafe escape_json_string_length
    :: BA# Word8 -> Int -> Int -> Int

foreign import ccall unsafe escape_json_string
    :: BA# Word8 -> Int -> Int -> MBA# Word8 -> Int -> IO Int
