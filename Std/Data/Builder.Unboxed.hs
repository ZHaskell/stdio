{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

{-|
Module      : Std.Data.Builder
Description : Efficient serialization/format.
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

If you're users looking for functions turn data into 'Builder' s, check 'Std.Data.Binary' or
'Std.Data.Textual'. This module is intend to 'Builder' builders.

A 'Builder' records a buffer writing function, which can be 'mappend' in O(1) via composition.
In stdio a 'Builder' are designed to deal with different 'AllocateStrategy', it affects how
'Builder' react when writing across buffer boundaries:

  * When building a short strict 'Bytes' with 'buildBytes/buildByteswith',
    we do a 'DoubleBuffer'.

  * When building a large lazy @[Bytes]@ with 'buildBytesList/buildBytesListwith',
    we do an 'InsertChunk'.

  * When building and consuming are interlaced with 'buildAndRun/buildAndRunWith',
    we do an 'OneShotAction'.

Most of the time using combinators from this module to build 'Builder' s is enough,
but in case of rolling something shining from the ground, keep an eye on correct
'AllocateStrategy' handling.

-}

module Std.Data.Builder where

import Control.Monad.Primitive
import GHC.Prim
import GHC.Types
import GHC.ST
import GHC.STRef
import GHC.IORef
import GHC.Magic (lazy)
import Control.Monad
import qualified Std.Data.Vector as V
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Data.Word
import Data.Bits (shiftL, shiftR)

-- | 'AllocateStrategy' will decide how each 'BuildStep' proceed when previous buffer is not enough.
--
data AllocateStrategy s
    = DoubleBuffer       -- Double the buffer and continue building
    | InsertChunk {-# UNPACK #-} !Int   -- Insert a new chunk and continue building
    | OneShotAction (V.Bytes -> State# s -> State# s)   -- Freeze current chunk and perform action with it.
                                        -- Use the 'V.Bytes' argument outside the action is dangerous
                                        -- since we will reuse the buffer after action finished.


-- | @BuilderStep@ is a function that fill buffer under given conditions.
--
-- We use unboxed 'State#' token here to support building inside both ST and IO monad
-- without touching 'unsafePerformIO'.
--
type BuildStep s = MutableByteArray# s -> Int# -> State# s -> (# State# s, [V.Bytes] #)

-- | @Builder@ is a monad to help compose @BuilderStep@. With next @BuilderStep@ continuation,
-- We can do interesting things like perform some action, or interleave the build process.
--
newtype Builder a = Builder { runBuilder ::
    forall s. AllocateStrategy s -> (a -> BuildStep s) -> BuildStep s}

instance Functor Builder where
    {-# INLINE fmap #-}
    fmap f (Builder b) = Builder (\ al k -> b al (k . f))
    {-# INLINE (<$) #-}
    a <$ (Builder b) = Builder (\ al k -> b al (\ _ -> k a))

instance Applicative Builder where
    {-# INLINE pure #-}
    pure x = Builder (\ _ k -> k x)
    {-# INLINE (<*>) #-}
    (Builder f) <*> (Builder b) = Builder (\ al k -> f al ( \ ab -> b al (k . ab)))
    {-# INLINE (*>) #-}
    (*>) = append

instance Monad Builder where
    {-# INLINE (>>=) #-}
    (Builder b) >>= f = Builder (\ al k -> b al ( \ a -> runBuilder (f a) al k))
    {-# INLINE (>>) #-}
    (>>) = append

--------------------------------------------------------------------------------

lastStep_ :: a -> MutableByteArray# s -> Int# -> State# s -> (# State# s, [V.Bytes] #)
{-# INLINE lastStep_ #-}
lastStep_ x buf# offset# s0# =
    let (# s1#, siz# #) = getSizeofMutableByteArray# buf# s0#
        s2# = case offset# <# siz# of
            1# -> shrinkMutableByteArray# buf# offset# s1#
            _ -> s1#
        (# s3#, ba# #) = unsafeFreezeByteArray# buf# s2#
    in (# s3#, [V.PrimVector (PrimArray ba#) 0 (I# offset#)] #)

lastStep :: MutVar# s a -> a -> MutableByteArray# s -> Int# -> State# s -> (# State# s, [V.Bytes] #)
{-# INLINE lastStep #-}
lastStep var# x buf# offset# s0# =
    let (# s1#, siz# #) = getSizeofMutableByteArray# buf# s0#
        s2# = case offset# <# siz# of
            1# -> shrinkMutableByteArray# buf# offset# s1#
            _  -> s1#
        (# s3#, ba# #) = unsafeFreezeByteArray# buf# s2#
        s4# = writeMutVar# var# x s3#
    in (# s4#, [V.PrimVector (PrimArray ba#) 0 (I# offset#)] #)

buildBytes_ :: Builder a -> V.Bytes
{-# INLINABLE buildBytes_ #-}
buildBytes_ = buildBytesWith_ V.defaultInitSize

buildBytes :: Builder a -> (V.Bytes, a)
{-# INLINABLE buildBytes #-}
buildBytes = buildBytesWith V.defaultInitSize

buildBytesWith_ :: Int -> Builder a -> V.Bytes
{-# INLINABLE buildBytesWith_ #-}
buildBytesWith_ initSiz (Builder b) = runST (do
    (MutableByteArray buf#) <- newByteArray initSiz
    [ba] <- primitive (b DoubleBuffer lastStep_ buf# 0#)
    return ba)

buildBytesWith :: Int -> Builder a -> (V.Bytes, a)
{-# INLINABLE buildBytesWith #-}
buildBytesWith initSiz (Builder b) = runST (do
    (MutableByteArray buf#) <- newByteArray initSiz
    ref@(STRef ref#) <- newSTRef undefined  -- kinda hack but easy
    [ba] <- primitive (b DoubleBuffer (lastStep ref#) buf# 0#)
    a <- readSTRef ref
    return (ba, a))


buildBytesList :: Builder a -> ([V.Bytes], a)
buildBytesList = buildBytesListWith  V.smallChunkSize V.defaultChunkSize

buildBytesListWith :: Int -> Int -> Builder a -> ([V.Bytes], a)
buildBytesListWith initSiz chunkSiz (Builder b) = runST (do
    (MutableByteArray buf#) <- newByteArray initSiz
    ref@(STRef ref#) <- newSTRef undefined  -- kinda hack but easy
    bs <- primitive (b (InsertChunk chunkSiz) (lastStep ref#) buf# 0#)
    a <- readSTRef ref
    return (bs, a))
{-# INLINABLE buildBytesList #-}


buildAndRun_ :: (V.Bytes -> IO ()) -> Builder a -> IO ()
buildAndRun_ = buildAndRunWith_ V.defaultChunkSize

buildAndRunWith_ :: Int -> (V.Bytes -> IO ()) -> Builder a -> IO ()
{-# INLINABLE buildAndRun_ #-}
buildAndRunWith_ chunkSiz f (Builder b) = do
    (MutableByteArray buf#) <- newByteArray chunkSiz
    (void . primitive) (b (OneShotAction g#) lastStep_ buf# 0#)
  where
    g# bs s0# = case f bs of IO f# -> case f# s0# of (# s1#, _ #) -> s1#
    lastStep_ _ buf# offset# s0# =
        let (# s1#, ba# #) = unsafeFreezeByteArray# buf# s0#
            IO f# = f (V.PrimVector (PrimArray ba#) 0 (I# offset#))
            (# s2#, _ #) = f# s1#
        in (# s2#, [] #) -- to match the silly return type

buildAndRun :: (V.Bytes -> IO ()) -> Builder a -> IO a
buildAndRun = buildAndRunWith V.defaultChunkSize

buildAndRunWith :: Int -> (V.Bytes -> IO ()) -> Builder a -> IO a
{-# INLINABLE buildAndRun #-}
buildAndRunWith chunkSiz f (Builder b) = do
    (MutableByteArray buf#) <- newByteArray chunkSiz
    ref@(IORef (STRef ref#)) <- newIORef undefined
    _ <- primitive (b (OneShotAction g#) (lastStep ref#) buf# 0#)
    readIORef ref
  where
    g# bs s0# = case f bs of IO f# -> case f# s0# of (# s1#, _ #) -> s1#
    lastStep var# x buf# offset# s0# =
        let (# s1#, ba# #) = unsafeFreezeByteArray# buf# s0#
            IO f# = f (V.PrimVector (PrimArray ba#) 0 (I# offset#))
            (# s2#, _ #) = f# s1#
            s3# = writeMutVar# var# x s2#
        in (# s3#, [] #) -- to match the silly return type

--------------------------------------------------------------------------------

append :: Builder a -> Builder b -> Builder b
{-# INLINE append #-}
append (Builder f) (Builder g) = Builder (\ al k -> f al ( \ _ ->  g al k))

empty :: Builder ()
{-# INLINE empty #-}
empty = Builder (\ _ k -> k ())

encodePrim :: Int#  -- ^ size bound
           -> (forall s. MutableByteArray# s -> Int# -> State# s -> State# s) -- ^ the writer which return a new offset
                                                                    -- for next write
           -> Builder ()
{-# INLINE encodePrim #-}
encodePrim n# f = ensureN n# `append`
    Builder (\ _ k buf# offset# s0# ->
        let s1# = f buf# offset# s0#
        in k () buf# (offset# +# n#) s1#)

--------------------------------------------------------------------------------
--
bytes :: V.Bytes -> Builder ()
{-# INLINE bytes #-}
bytes bs@(V.PrimVector (PrimArray arr#) (I# arroff#) (I# arrlen#)) =
    Builder (\ strategy k buf# offset# s0# -> case strategy of
        DoubleBuffer -> copy strategy k buf# offset# s0#
        InsertChunk (I# chunkSiz#) ->
            case arrlen# <=# chunkSiz# `uncheckedIShiftRL#` 1# of
                -- the copy limit is half the chunk size
                1# -> copy strategy k buf# offset# s0#
                _  -> insertChunk chunkSiz# 0#
                        (\ _ buf2# offset2# s1# ->
                            let (# s2#, bss #) = k () buf2# offset2# s1#
                            in (# s2#, bs:bss #))
                        buf# offset# s0#

        OneShotAction f# ->
            let (# s1#, chunkSiz# #) = getSizeofMutableByteArray# buf# s0#
            in case arrlen# <=# chunkSiz# `uncheckedIShiftRL#` 1# of
                -- the copy limit is half the chunk size
                1# -> copy strategy k buf# offset# s0#
                _  -> oneShotAction f# 0#
                        (\ _ buf2# offset2# s2# ->
                            let s3# = f# bs s2#
                            in (k () buf2# offset2# s3#))
                        buf# offset# s1#)
  where
    copy :: AllocateStrategy s -> (() -> BuildStep s) -> BuildStep s
    {-# INLINE copy #-}
    copy strategy k =
        runBuilder (ensureN arrlen#) strategy ( \ _ buf# offset# s0# ->
            let s1# = copyByteArray# arr# arroff# buf# offset# arrlen# s0#
            in k () buf# (offset# +# arrlen#) s1#)

-- | Ensure that there are at least @n@ many elements available.
ensureN :: Int# -> Builder ()
{-# INLINE ensureN #-}
ensureN n# = Builder $ \ strategy k buf# offset# s0# ->
    -- You may think doing this will be slow
    -- but this value lives in CPU cache for most of the time
    -- and we really have no way to force it to live in register
    -- in haskell
    let (# s1#, siz# #) = getSizeofMutableByteArray# buf# s0#
    in case siz# -# offset# >=# n# of
        1# -> k () buf# offset# s0#
        _  -> handleBoundary strategy n# k buf# offset# s0#
  where
    -- | Handle chunk boundary
    -- This code is too large, manually force not to be inlined
    {-# NOINLINE handleBoundary #-}
    handleBoundary DoubleBuffer = doubleBuffer
    handleBoundary (InsertChunk (I# chunkSiz#)) = insertChunk chunkSiz#
    handleBoundary (OneShotAction action) = oneShotAction action

doubleBuffer :: Int# -> (() -> BuildStep s) -> BuildStep s
{-# INLINE doubleBuffer #-}
doubleBuffer wantSiz# k buf# offset# s0# =
    let siz# = (uncheckedIShiftL# offset# 1# +# wantSiz#)
    -- double the buffer
        (# s1#, buf2# #) = resizeMutableByteArray# buf# siz# s0#
    in k () buf# offset# s1#                 -- continue building

insertChunk :: Int# -> Int# -> (() -> BuildStep s) -> BuildStep s
{-# INLINE insertChunk #-}
insertChunk chunkSiz# wantSiz# k buf# offset# s0# =
    let (# s1#, siz# #) = getSizeofMutableByteArray# buf# s0#
    in case offset# /=# 0# of
        -- this holds mostly, unless somebody want a large chunk at beginning
        1# ->
            let s2# = case offset# <# siz# of
                    -- shrink old buffer if not full
                    1# -> shrinkMutableByteArray# buf# offset# s1#
                    _  -> s1#
                -- popup old buffer
                (# s3#, ba# #) = unsafeFreezeByteArray# buf# s2#
                -- make a new buffer
                (# s4#, buf2# #) = newByteArray# (case wantSiz# <=# chunkSiz# of
                                                    1# -> chunkSiz#
                                                    _  -> wantSiz#) s3#
                -- delay the rest building process
                xs = case (k () buf2# 0# s4#) of (# _, res #) -> lazy res
            in (# s4#, (V.PrimVector (PrimArray ba#) 0 (I# offset#) : xs) #)
        _  ->
            -- this holds certainly, but we still guard it
            case wantSiz# ># siz# of
                1# ->
                    -- make a new buffer
                    let (# s1#, buf2# #) = newByteArray# wantSiz# s0#
                    in k () buf2# 0# s1#
                _  -> k () buf# 0# s0#

oneShotAction :: (V.Bytes ->  State# s -> State# s) -> Int# -> (() -> BuildStep s) -> BuildStep s
{-# INLINE oneShotAction #-}
oneShotAction f# wantSiz# k buf# offset# s0# =
    let (# s1#, siz# #) = getSizeofMutableByteArray# buf# s0#
    in case offset# /=# 0# of
        -- this holds mostly, unless somebody want a large chunk at beginning
        1# ->
                -- popup old buffer
            let (# s2#, ba# #) = unsafeFreezeByteArray# buf# s1#
                -- run one shot action
                s3# = f# (V.PrimVector (PrimArray ba#) 0 (I# offset#)) s2#
                -- make a new buffer
                (# s4#, buf2# #) = newByteArray# (case wantSiz# <=# siz# of
                                                    1# -> siz#
                                                    _  -> wantSiz#) s3#
            in k () buf# 0# s4#
        _  ->
            -- this holds certainly, but we still guard it
            case wantSiz# ># siz# of
                1# ->
                    -- make a new buffer
                    let (# s1#, buf2# #) = newByteArray# wantSiz# s0#
                    in k () buf2# 0# s1#
                _  -> k () buf# 0# s0#


--------------------------------------------------------------------------------
