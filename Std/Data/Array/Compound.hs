{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UnboxedTuples      #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE DeriveDataTypeable           #-}
{-# LANGUAGE StandaloneDeriving           #-}
{-# LANGUAGE FlexibleInstances           #-}

{-|
Module      : Std.Data.Array.Checked
Description : Bounded checked boxed and unboxed arrays
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides 'CompoundArray', i.e. array composed with two different type of arrays. It provides different memory characteristic from array of tuples. e.g. the memory layout of @Array (Int, Foo)@ may looks like this:

@
    +-----+---+   +------+---+---+----
    |Array| * +-->+Array#| # | * | ...
    +-----+---+   +------+---+-+-+----
                               |
                               V
                             +-+-+---+---+    +---+-----
                             |(,)| * | * +--->+Foo| ...
                             +---+-+-+---+    +---+-----
                                   |
                                   V
                                 +-+-+---+
                                 |Int| # |
                                 +---+-+-+
@

With @CompoundArray PrimArray Array (Int, Foo)@, things turn into:

@

    +---------------+---+---+   +-----+---+---+----
    | CompoundArray | * | * +-->+Array| # | * | ...
    +---------------+-+-+---+   +-----+---+-+-+----
                      |                     |       +---+-----
                      |                     +------>|Foo| ...
                      |
                      V
                    +---------+---+---+----
                    |PrimArray| * + * | ...
                    +---------+-+-+-+-+----
@

So

-}
module Std.Data.Array.Compound where

import Std.Data.Array
import Control.Applicative
import Data.Data
import Data.Typeable

-- Instead of providing a generalized compound array with polymorphric array fields, we use this typeclass
-- so that instances use concrete array type can unpack their array payload.
class (Arr marrA arrA a, Arr marrB arrB b) => CompoundArr marr arr a b | marr -> arr, arr -> marr  where
    composedArr :: arrA a -> arrB b -> arr a b
    composedMutArr :: marrA s a -> marrB s b -> marr s a b

data CompoundArrayArray x y = CompoundArrayArray (Array x) (Array y)
data CompoundMutableArrayArray s x y = CompoundMutableArrayArray (MutableArray s x) (MutableArray s y)


instance (CompoundArr marr arr a b)
    => Arr (MutableCompoundArray marrA marrB) (CompoundArray arrA arrB) (x, y) where
    type MArr (CompoundArray arrA arrB) = MutableCompoundArray (MArr arrA) (MArr arrB)
    type IArr (MutableCompoundArray marrA marrB) = CompoundArray (IArr marrA) (IArr marrB)

    newArr n = liftA2 MutableCompoundArray (newArr n) (newArr n)
    {-# INLINE newArr #-}
    newArrWith n (x,y) = liftA2 MutableCompoundArray (newArrWith n x) (newArrWith n y)
    {-# INLINE newArrWith #-}
    readArr (MutableCompoundArray marrA marrB) i = liftA2 (,) (readArr marrA i) (readArr marrB i)
    {-# INLINE readArr #-}
    writeArr (MutableCompoundArray marrA marrB) i (x,y) = do
        writeArr marrA i x
        writeArr marrB i y
    {-# INLINE writeArr #-}
    setArr (MutableCompoundArray marrA marrB) s l (x,y) = do
        setArr marrA s l x
        setArr marrB s l y
    {-# INLINE setArr #-}
    indexArr (CompoundArray arrA arrB) i = (indexArr arrA i, indexArr arrB i)
    {-# INLINE indexArr #-}
    indexArr' (CompoundArray arrA arrB) i =
        let (# x #) = indexArr' arrA i
            (# y #) = indexArr' arrB i
        in (# (x,y) #)
    {-# INLINE indexArr' #-}
    indexArrM (CompoundArray arrA arrB) i = liftA2 (,) (indexArrM arrA i) (indexArrM arrB i)
    {-# INLINE indexArrM #-}
    freezeArr (MutableCompoundArray marrA marrB) i j =
        liftA2 CompoundArray (freezeArr marrA i j) (freezeArr marrB i j)
    {-# INLINE freezeArr #-}
    thawArr (CompoundArray arrA arrB) i j =
        liftA2 MutableCompoundArray (thawArr arrA i j) (thawArr arrB i j)
    {-# INLINE thawArr #-}
    unsafeFreezeArr (MutableCompoundArray marrA marrB) =
        liftA2 CompoundArray (unsafeFreezeArr marrA) (unsafeFreezeArr marrB)
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr (CompoundArray arrA arrB) =
        liftA2 MutableCompoundArray (unsafeThawArr arrA) (unsafeThawArr arrB)
    {-# INLINE unsafeThawArr #-}

    copyArr (MutableCompoundArray marrA marrB) s (CompoundArray arrA arrB) i j = do
        copyArr marrA s arrA i j
        copyArr marrB s arrB i j
    {-# INLINE copyArr #-}

    copyMutableArr (MutableCompoundArray marrA marrB) s (MutableCompoundArray marrC marrD) i j = do
        copyMutableArr marrA s marrC i j
        copyMutableArr marrB s marrD i j
    {-# INLINE copyMutableArr #-}

    moveArr (MutableCompoundArray marrA marrB) s (MutableCompoundArray marrC marrD) i j = do
        moveArr marrA s marrC i j
        moveArr marrB s marrD i j
    {-# INLINE moveArr #-}

    cloneArr (CompoundArray arrA arrB) s l =
        CompoundArray (cloneArr arrA s l) (cloneArr arrB s l)
    {-# INLINE cloneArr #-}

    cloneMutableArr (MutableCompoundArray marrA marrB) s l =
        liftA2 MutableCompoundArray (cloneMutableArr marrA s l) (cloneMutableArr marrB s l)
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr (MutableCompoundArray marrA marrB) n =
        liftA2 MutableCompoundArray (resizeMutableArr marrA n) (resizeMutableArr marrB n)
    {-# INLINE resizeMutableArr #-}

    shrinkMutableArr (MutableCompoundArray marrA marrB) n = do
        shrinkMutableArr marrA n
        shrinkMutableArr marrB n
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr (MutableCompoundArray marrA marrB) (MutableCompoundArray marrC marrD) =
        sameMutableArr marrA marrC && sameMutableArr marrB marrD
    {-# INLINE sameMutableArr #-}

    -- two array part should have same size
    sizeofArr (CompoundArray arrA _) = sizeofArr arrA
    {-# INLINE sizeofArr #-}
    sizeofMutableArr (MutableCompoundArray marrA marrB) = sizeofMutableArr marrA
    {-# INLINE sizeofMutableArr #-}

    sameArr (CompoundArray arrA arrB) (CompoundArray arrC arrD) =
        sameArr arrA arrC && sameArr arrB arrD
    {-# INLINE sameArr #-}
