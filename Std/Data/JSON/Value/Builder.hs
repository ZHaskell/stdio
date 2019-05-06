{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UnliftedFFITypes   #-}
{-# LANGUAGE TypeApplications   #-}

{-|
Module      : Std.Data.JSON.Value.Builder
Description : JSON representation and builders
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides builders for JSON 'Value's, a Haskell JSON representation. These builders are designed to comply with <https://tools.ietf.org/html/rfc8258 rfc8258>. Only control characters are escaped, other unicode codepoints are directly written instead of being escaped.

-}
module Std.Data.JSON.Value.Builder
  ( -- * Value Builders
    value
  , object
  , object'
  , array
  , array'
  , string
    -- * Builder helpers
  , curly, square, quotes, (>:<), (>!<), (>+<)
  , commaVector, commaList
    -- * Re-export 'Value' type
  , Value(..)
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

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define COLON 58
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91

-- | add @{...}@ to original builder.
curly :: B.Builder () -> B.Builder ()
{-# INLINE curly #-}
curly b = B.encodePrim @Word8 OPEN_CURLY >> b >> B.encodePrim @Word8 CLOSE_CURLY

-- | add @[...]@ to original builder.
square :: B.Builder () -> B.Builder ()
{-# INLINE square #-}
square b = B.encodePrim @Word8 OPEN_SQUARE >> b >> B.encodePrim @Word8 CLOSE_SQUARE

-- | add @"..."@ to original builder.
quotes :: B.Builder () -> B.Builder ()
{-# INLINE quotes #-}
quotes b = B.encodePrim @Word8 DOUBLE_QUOTE >> b >> B.encodePrim @Word8 DOUBLE_QUOTE

-- | use @:@ to connect a field name(will not be escaped) and a field.
(>:<) :: T.Text -> B.Builder () -> B.Builder ()
{-# INLINE (>:<) #-}
l >:< b = do
    B.encodePrim @Word8 DOUBLE_QUOTE
    B.text l
    B.encodePrim @Word8 DOUBLE_QUOTE
    B.encodePrim @Word8 COLON
    b
infix 9 >:<

-- | use @:@ to connect a field name(possiblely contain chars need to be escaped) and a field.
(>!<) :: T.Text -> B.Builder () -> B.Builder ()
{-# INLINE (>!<) #-}
l >!< b = string l >> B.encodePrim @Word8 COLON >> b
infix 9 >!<

-- | use @,@ to connect seperated values or key-values.
(>+<) :: B.Builder () -> B.Builder () -> B.Builder ()
{-# INLINE (>+<) #-}
a >+< b = a >> B.encodePrim @Word8 COMMA >> b
infix 8 >+<

-- | Use @,@ as separator to connect list of builders.
commaList :: (a -> B.Builder ()) -> [a] -> B.Builder ()
{-# INLINE commaList #-}
commaList f xs = go xs
  where
    go [] = pure ()
    go [x] = f x
    go (x:xs) = f x >> B.encodePrim @Word8 COMMA >> go xs

-- | Use @,@ as separator to connect a vector of builders.
commaVector :: (V.Vec v a) => (a -> B.Builder ()) -> v a ->  B.Builder ()
{-# INLINE commaVector #-}
commaVector f v = do
    V.traverseVector_ (\ x -> f x >> B.encodePrim @Word8 COMMA) (V.initMayEmpty v)
    forM_ (V.lastMaybe v) f

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
{-# INLINE array #-}
array = square . commaVector value

array' :: (a -> B.Builder ()) -> V.Vector a -> B.Builder ()
{-# INLINE array' #-}
array' f = square . commaVector f

object :: V.Vector (T.Text, Value) -> B.Builder ()
{-# INLINE object #-}
object = curly . commaVector (\ (k, v) -> k >!< value v)

object' :: (a -> B.Builder ()) -> V.Vector (T.Text, a) -> B.Builder ()
{-# INLINE object' #-}
object' f = curly . commaVector (\ (k, v) -> k >!< f v)

string :: T.Text -> B.Builder ()
{-# INLINE string #-}
string (T.Text (V.PrimVector ba@(PrimArray ba#) s l)) = do
    let siz = escape_json_string_length ba# s l
    B.ensureN siz
    B.Builder (\ _  k (B.Buffer mba@(MutablePrimArray mba#) i) -> do
        if siz == l+2   -- no need to escape
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
