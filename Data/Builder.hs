{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Data.Builder where

import Control.Monad.Primitive
import GHC.Prim (RealWorld)
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Array as A
import Data.Monoid (Monoid(..))
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word
import Data.Bits (shiftL, shiftR)
import Data.Primitive
import Debug.Trace
import Control.Monad.ST.Unsafe
import System.IO.Unsafe (unsafeInterleaveIO, unsafeDupablePerformIO)

-- | 'AllocateStrategy' will decide how each 'BuildStep' proceed when previous buffer is not enough.
--
data AllocateStrategy
    = DoubleBuffer       -- Double the buffer and continue building
    | InsertChunk {-# UNPACK #-} !Int   -- Insert a new chunk and continue building
    | OneShotAction (V.Bytes -> IO ())   -- Freeze current chunk and perform action with it.
                                        -- Use the 'V.Bytes' argument outside the action is dangerous
                                        -- since we will reuse the buffer after action finished.

-- | Helper type to help ghc unpack
--
data Buffer = Buffer {-# UNPACK #-} !(A.MutablePrimArray RealWorld Word8)  -- well, the buffer content
                       {-# UNPACK #-} !Int  -- writing offset

-- | @BuilderStep@ is a function that fill buffer under given conditions.
--
type BuildStep = Buffer -> IO [V.Bytes]

-- | @Builder@ is a monoid to help compose @BuilderStep@. With next @BuilderStep@ continuation,
-- We can do interesting things like perform some action, or interleave the build process.
--
newtype Builder = Builder { runBuilder :: AllocateStrategy -> BuildStep -> BuildStep }

#if MIN_VERSION_base(4,9,0)
instance Semigroup Builder where
   (<>) = append
   {-# INLINE (<>) #-}
#endif

instance Monoid Builder where
   mempty  = empty
   {-# INLINE mempty #-}
#if MIN_VERSION_base(4,9,0)
   mappend = (<>) -- future-proof definition
#else
   mappend = append
#endif
   {-# INLINE mappend #-}
   mconcat = foldr append empty
   {-# INLINE mconcat #-}

append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder (\ a -> f a . g a)
{-# INLINE append #-}

empty :: Builder
empty = Builder (\ _ -> id)
{-# INLINE empty #-}


atMost :: Int  -- ^ size bound
          -> (A.MutablePrimArray (PrimState IO) Word8 -> Int -> IO Int)  -- ^ the writer which return a new offset
                                                                           -- for next write
          -> Builder
atMost n f = ensureFree n `append`
    Builder (\ _  k (Buffer buf offset ) ->
        f buf offset >>= \ offset' -> k (Buffer buf offset')
    )
{-# INLINE atMost #-}

--------------------------------------------------------------------------------
--
bytes :: V.Bytes -> Builder
bytes bs@(V.PrimVector arr s l) = Builder go
  where
    go strategy k buffer@(Buffer buf offset) = case strategy of
        DoubleBuffer -> copy strategy k buffer
        InsertChunk chunkSiz
            | l <= chunkSiz `shiftR` 1 ->
                copy strategy k buffer -- the copy limit is half the chunk size
            | offset /= 0 ->
                 insertChunk chunkSiz 0 (\ buffer' -> (bs:) `fmap` k buffer') buffer
            | otherwise -> (bs:) `fmap` k buffer
        OneShotAction action -> do
            chunkSiz <- A.sizeofMutableArr buf
            case () of
                _
                    | l <= chunkSiz `shiftR` 1 ->
                        copy strategy k buffer
                    | offset /= 0 ->
                        oneShotAction action 0 (\ buffer' -> action bs >> k buffer') buffer
                    | otherwise -> action bs >> k buffer
    {-# NOINLINE go #-}
    copy strategy k =
        runBuilder (ensureFree l) strategy ( \ (Buffer buf offset) -> do
                A.copyArr buf offset arr s l
                k (Buffer buf (offset+l))
            )
    {-# INLINE copy #-}
{-# INLINE bytes #-}

-- | Ensure that there are at least @n@ many elements available.
ensureFree :: Int -> Builder
ensureFree !n = Builder $ \ strategy k buffer@(Buffer buf offset) -> do
    siz <- A.sizeofMutableArr buf  -- You may think doing this will be slow
                                   -- but this value lives in CPU cache for most of the time
    if siz - offset >= n
    then k buffer
    else handleBoundary strategy n k buffer
  where
    handleBoundary DoubleBuffer n k buffer = doubleBuffer n k buffer
    handleBoundary (InsertChunk chunkSiz) n k buffer = insertChunk chunkSiz n k buffer
    handleBoundary (OneShotAction action) n k buffer = oneShotAction action n k buffer
    {-# NOINLINE handleBoundary #-}
{-# INLINE ensureFree #-}

--------------------------------------------------------------------------------
--
-- | Handle chunk boundary
--
doubleBuffer :: Int -> BuildStep -> BuildStep
doubleBuffer !wantSiz k buffer@(Buffer buf offset) = do
    !siz <- A.sizeofMutableArr buf
    let !siz' = max (offset + wantSiz `shiftL` 1)
                    (siz `shiftL` 1)
    buf' <- A.resizeMutableArr buf siz'   -- double the buffer
    k (Buffer buf' offset)                 -- continue building
{-# INLINE doubleBuffer #-}

insertChunk :: Int -> Int -> BuildStep -> BuildStep
insertChunk !chunkSiz !wantSiz k buffer@(Buffer buf offset) = do
    !siz <- A.sizeofMutableArr buf
    case () of
        _
            | offset /= 0 -> do     -- this is certainly hold, but we still guard it
                when (offset < siz)
                    (A.shrinkMutableArr buf offset)            -- shrink old buffer if not full
                arr <- A.unsafeFreezeArr buf                   -- popup old buffer
                buf' <- A.newArr (max wantSiz chunkSiz)        -- make a new buffer
                xs <- unsafeInterleaveIO (k (Buffer buf' 0))  -- delay the rest building process
                let v = V.fromArr arr 0 offset
                v `seq` return (v : xs)
            | wantSiz <= siz -> k (Buffer buf 0)
            | otherwise -> do
                buf' <- A.newArr wantSiz        -- make a new buffer
                k (Buffer buf' 0 )
{-# INLINE insertChunk #-}

oneShotAction :: (V.Bytes -> IO ()) -> Int -> BuildStep -> BuildStep
oneShotAction action !wantSiz k buffer@(Buffer buf offset) = do
    !siz <- A.sizeofMutableArr buf
    case () of
        _
            | offset /= 0 -> do
                arr <- A.unsafeFreezeArr buf             -- popup old buffer
                action (V.PrimVector arr 0 offset)
                if wantSiz <= siz
                then k (Buffer buf 0)                    -- continue building with old buf
                else do
                    buf' <- A.newArr wantSiz             -- make a new buffer
                    k (Buffer buf' 0)
            | wantSiz <= siz -> k (Buffer buf 0)
            | otherwise -> do
                buf' <- A.newArr wantSiz                -- make a new buffer
                k (Buffer buf' 0 )
{-# INLINE oneShotAction #-}

--------------------------------------------------------------------------------

buildBytes :: Builder -> V.Bytes
buildBytes = buildBytesWith V.defaultInitSize

buildBytesWith :: Int -> Builder -> V.Bytes
buildBytesWith initSiz (Builder b) = unsafeDupablePerformIO $ do
    buf <- A.newArr initSiz
    [bs] <- b DoubleBuffer lastStep (Buffer buf 0 )
    return bs
  where
    lastStep :: Buffer -> IO [V.PrimVector Word8]
    lastStep (Buffer buf offset) = do
        siz <- A.sizeofMutableArr buf
        when (offset < siz) (A.shrinkMutableArr buf offset)
        arr <- A.unsafeFreezeArr buf
        return [V.PrimVector arr 0 offset]
{-# INLINABLE buildBytes #-}


buildBytesList :: Builder -> [V.Bytes]
buildBytesList = buildBytesListWith  V.smallChunkSize V.defaultChunkSize

buildBytesListWith :: Int -> Int -> Builder -> [V.Bytes]
buildBytesListWith initSiz chunkSiz (Builder b) = unsafeDupablePerformIO $ do
    buf <- A.newArr initSiz
    b (InsertChunk chunkSiz) lastStep (Buffer buf 0)
  where
    lastStep (Buffer buf offset) = do
        arr <- A.unsafeFreezeArr buf
        return [V.PrimVector arr 0 offset]
{-# INLINABLE buildBytesList #-}

buildAndRun :: (V.Bytes -> IO ()) -> Builder -> IO ()
buildAndRun = buildAndRunWith V.defaultChunkSize

buildAndRunWith :: Int -> (V.Bytes -> IO ()) -> Builder -> IO ()
buildAndRunWith chunkSiz action (Builder b) = do
    buf <- A.newArr chunkSiz
    _ <- b (OneShotAction action) lastStep (Buffer buf 0)
    return ()
  where
    lastStep (Buffer buf offset) = do
        arr <- A.unsafeFreezeArr buf
        action (V.PrimVector arr 0 offset)
        return [] -- to match the silly return type
{-# INLINABLE buildAndRun #-}
