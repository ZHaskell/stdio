{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : Std.Data.Vector
Description : Fast boxed and unboxed vector
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide fast boxed and unboxed vector with unified interface. Conceptually a vector is simply a slice of an array, for example this is the definition of boxed vector:

@
    data Vector a = Vector
        {-# UNPACK #-} !(SmallArray a) -- payload
        {-# UNPACK #-} !Int         -- offset
        {-# UNPACK #-} !Int         -- length
@

The 'Vec' class unified different type of vectors, and this module provide operation over 'Vec' instances. The API is similar to bytestring and vector. If you find missing functions, please report!

Performance consideration:

  * Use 'PrimVector' for 'Prim' types, it stores content in packed memory, and it's
    strict on its elements (following strictness consideration are mainly for lifted
    'Vector' type), many functions DO NOT NEED the result vectors's type to be same
    with the source one, e.g. @map :: (Vec v a, Vec u b) => (a -> b) -> v a -> u b@.

  * Both 'PrimVector' and 'Vector' are instances of 'Foldable', so 'null', 'length',
    and various folds from 'Prelude' can work on them without performance cost,
    specialized versions are only provided from 'Std.Data.Vector.Internal' module.

  * The 'Functor' instances for 'Vector' are lazy in order to abid 'Functor' law.
    namely @fmap id vectorConatin_|_ == vectorContain_|_@, if you need strict mapping
    for lifted 'Vector', use 'map' ('PrimVector' will never contain _|_ thus it's not
    a problem). THIS MAY COME AS A SURPRISE SO MAKE SURE YOU USE THE CORRECT 'map' s.

  * The 'Traversable' instances have specialized implementation for 'ST' and 'IO',
    to make sure you don't write thunks into result, use 'return <$!>' idiom.

  * When use state generating functions like 'mapAccumL', 'mapAccumR' ,etc. force
    both the accumulator and value with @acc `seq` v `seq` (acc, v)@ idiom to avoid
    thunks inside result vector.

  * 'scanl' and 'scanr' are strict in its accumulator, use a list and then 'pack' it
    to perform a lazy scan, (generally we prefer lazy version when there are strict
    variations or if user can avoid space leak by adding extra 'seq' s, but in case
    like this users have no way to control strictness, we would choose strict).

  * The 'unpack' / 'unpackR' and 'pack' / 'packN' / 'packR' / packRN' are designed to
    work with @build/foldr@ streaming fusion in base, thus it's OK to expect idioms like
    @pack . List filter f . List.map . unpack@ to work in contant space. While
    @Vector.filter . Vector.map@ will create intermediate vectors on the fly
    (which have different time/space characteristic).

  * The 'elem' series functions have a specialized implementation for 'PrimVector Word8',
    i.e. the 'Bytes' type, which use 'memchr' (which in turn use SIMD instructions on
    most platforms), so avoid @find (== w)@ if possible.

-}

module Std.Data.Vector (
  -- * Vec typeclass
    Vec
  -- * Boxed and unboxed vector type
  , Vector
  , PrimVector
  -- ** 'Word8' vector
  , Bytes, w2c, c2w
  -- * Basic creating
  , empty, singleton, copy
  -- * Conversion between list
  , pack, packN, packR, packRN
  , unpack, unpackR
  -- * Basic interface
  , append
  , null
  , length
  , cons, snoc, uncons, unsnoc
  , head, tail
  , last, init
  -- * Transforming vector
  , map
  , reverse
  , intersperse
  , intercalate
  , intercalateElem
  , transpose
  -- * Reducing vector (folds)
  , foldl1'
  , foldr1'
    -- ** Special folds
  , concat
  , concatMap
  , maximum
  , minimum
  -- * Building vector
  -- ** Scans
  , scanl
  , scanl1
  , scanr
  , scanr1
  -- ** Accumulating maps
  , mapAccumL
  , mapAccumR
  -- ** Generating and unfolding vector
  , replicate
  , unfoldr
  , unfoldrN
  -- * Substrings
  , take
  , drop
  , slice
  , splitAt



  -- * Searching vectors

  -- ** Searching by equality
  , elem
  , notElem
  , elemIndex
  -- ** Searching with a predicate
  , find
  , filter
  , partition

  -- * literal quoters
  , vecW8
  , vecW16
  , vecW32
  , vecW64
  , vecWord
  , vecI8
  , vecI16
  , vecI32
  , vecI64
  , vecInt
  , ascii

 ) where

import           Prelude ()
import           Std.Data.Vector.Base
import           Std.Data.Vector.QQ         as QQ

