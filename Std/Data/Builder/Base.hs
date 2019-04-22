{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}

{-|
Module      : Std.Data.Builder.Base
Description : Efficient serialization/format.
Copyright   : (c) Dong Han, 2017-2019
              (c) Tao He, 2018-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

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

module Std.Data.Builder.Base
  ( -- * Builder type
    AllocateStrategy(..)
  , Buffer(..)
  , BuildStep
  , Builder(..)
  , append
   -- * Running a builder
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
   -- * Boundary handling
  , doubleBuffer
  , insertChunk
  , oneShotAction
   -- * Pritimive builders
  , encodePrim
  , encodePrimLE
  , encodePrimBE
  -- * More builders
  , stringModifiedUTF8, charModifiedUTF8, stringUTF8, charUTF8, string7, char7, string8, char8, text
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe            (unsafeInterleaveST)
import           Data.Bits                          (shiftL, shiftR, (.&.))
import           Data.Monoid                        (Monoid (..))
import           Data.Primitive.PrimArray           (MutablePrimArray (..))
import           Data.Primitive.Ptr                 (copyPtrToMutablePrimArray)
import           Data.Semigroup                     (Semigroup (..))
import           Data.String                        (IsString (..))
import           Data.Word
import           Data.Int
import           GHC.CString                        (unpackCString#, unpackCStringUtf8#)
import           GHC.Prim
import           GHC.Ptr
import           GHC.Types
import qualified Std.Data.Array                     as A
import           Std.Data.PrimArray.UnalignedAccess
import qualified Std.Data.Text.Base                 as T
import qualified Std.Data.Text.UTF8Codec            as T
import qualified Std.Data.Vector.Base               as V
import           System.IO.Unsafe
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))

-- | 'AllocateStrategy' will decide how each 'BuildStep' proceed when previous buffer is not enough.
--
data AllocateStrategy s
    = DoubleBuffer       -- Double the buffer and continue building
    | InsertChunk {-# UNPACK #-} !Int   -- Insert a new chunk and continue building
    | OneShotAction (V.Bytes -> ST s ())  -- Freeze current chunk and perform action with it.
                                        -- Use the 'V.Bytes' argument outside the action is dangerous
                                        -- since we will reuse the buffer after action finished.

-- | Helper type to help ghc unpack
--
data Buffer s = Buffer {-# UNPACK #-} !(A.MutablePrimArray s Word8)  -- ^ the buffer content
                       {-# UNPACK #-} !Int  -- ^ writing offset

-- | @BuilderStep@ is a function that fill buffer under given conditions.
--
type BuildStep s = Buffer s -> ST s [V.Bytes]

-- | @Builder@ is a monad to help compose @BuilderStep@. With next @BuilderStep@ continuation,
-- we can do interesting things like perform some action, or interleave the build process.
--
-- Notes on 'IsString' instance: @Builder ()@'s 'IsString' instance use 'stringModifiedUTF8',
-- which is different from 'stringUTF8' in that it DOES NOT PROVIDE UTF8 GUARANTEES! :
--
-- * @\NUL@ will be written as @\xC0 \x80@.
-- * @\xD800@ ~ @\xDFFF@ will be encoded in three bytes as normal UTF-8 codepoints.
--
newtype Builder a = Builder
    { runBuilder :: forall s. AllocateStrategy s -> (a -> BuildStep s) -> BuildStep s}

instance Show (Builder a) where
    show = show . buildBytes

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

instance Semigroup (Builder ()) where
    (<>) = append
    {-# INLINE (<>) #-}

instance Monoid (Builder ()) where
    mempty = pure ()
    {-# INLINE mempty #-}
    mappend = append
    {-# INLINE mappend #-}
    mconcat = foldr append (pure ())
    {-# INLINE mconcat #-}

instance (a ~ ()) => IsString (Builder a) where
    {-# INLINE fromString #-}
    fromString = stringModifiedUTF8

instance Arbitrary (Builder ()) where
    arbitrary = bytes <$> arbitrary
    shrink b = (bytes . V.pack) <$> shrink (V.unpack (buildBytes b))

instance CoArbitrary (Builder ()) where
    coarbitrary = coarbitrary . buildBytes

-- | Encode string with modified UTF-8 encoding, will be rewritten to a memcpy if possible.
stringModifiedUTF8 :: String -> Builder ()
{-# INLINE CONLIKE [0] stringModifiedUTF8 #-}
{-# RULES
    "stringModifiedUTF8/packAddrModified" forall addr . stringModifiedUTF8 (unpackCString# addr) = packAddrModified addr
  #-}
{-# RULES
    "stringModifiedUTF8/packAddrModified" forall addr . stringModifiedUTF8 (unpackCStringUtf8# addr) = packAddrModified addr
  #-}
stringModifiedUTF8 = mapM_ charModifiedUTF8

-- | Turn 'Char' into 'Builder' with Modified UTF8 encoding
--
-- '\NUL' is encoded as two bytes @C0 80@ , '\xD800' ~ '\xDFFF' is encoded as a three bytes normal UTF-8 codepoint.
charModifiedUTF8 :: Char -> Builder ()
{-# INLINE charModifiedUTF8 #-}
charModifiedUTF8 chr = do
    ensureN 4
    Builder (\ _  k (Buffer mba i) -> do
        i' <- T.encodeCharModifiedUTF8 mba i chr
        k () (Buffer mba i'))

packAddrModified :: Addr# -> Builder ()
packAddrModified addr# = copy addr#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr#
    copy addr# = do
        ensureN len
        Builder (\ _  k (Buffer mba i) -> do
           copyPtrToMutablePrimArray mba i (Ptr addr#) len
           k () (Buffer mba (i + len)))

append :: Builder a -> Builder b -> Builder b
{-# INLINE append #-}
append (Builder f) (Builder g) = Builder (\ al k -> f al ( \ _ ->  g al k))

--------------------------------------------------------------------------------

-- | Write a 'V.Bytes'.
bytes :: V.Bytes -> Builder ()
{-# INLINE bytes #-}
bytes bs@(V.PrimVector arr s l) = Builder (\ strategy k buffer@(Buffer buf offset) ->
    case strategy of
        DoubleBuffer -> copy strategy k buffer
        InsertChunk chunkSiz
            | l <= chunkSiz `shiftR` 1 ->
                copy strategy k buffer -- the copy limit is half the chunk size
            | offset /= 0 ->
                 insertChunk chunkSiz 0 (\ buffer' -> (bs:) `fmap` k () buffer') buffer
            | otherwise -> (bs:) `fmap` k () buffer
        OneShotAction action -> do
            chunkSiz <- A.sizeofMutableArr buf
            case () of
                _
                    | l <= chunkSiz `shiftR` 1 ->
                        copy strategy k buffer
                    | offset /= 0 ->
                        oneShotAction action 0 (\ buffer' -> action bs >> k () buffer') buffer
                    | otherwise -> action bs >> k () buffer)
  where
    copy :: forall s a. AllocateStrategy s -> (() -> BuildStep s) -> BuildStep s
    copy strategy k =
        runBuilder (ensureN l) strategy ( \ _ (Buffer buf offset) -> do
                A.copyArr buf offset arr s l
                k () (Buffer buf (offset+l)))
    {-# INLINE copy #-}

-- | Ensure that there are at least @n@ many elements available.
ensureN :: Int -> Builder ()
{-# INLINE ensureN #-}
ensureN !n = Builder $ \ strategy k buffer@(Buffer buf offset) -> do
    siz <- A.sizeofMutableArr buf  -- You may think doing this will be slow
                                   -- but this value lives in CPU cache for most of the time
    if siz - offset >= n
    then k () buffer
    else handleBoundary strategy n k buffer
  where
    {-# NOINLINE handleBoundary #-} -- Don't inline this branchy code
    handleBoundary DoubleBuffer n k buffer = doubleBuffer n (k ()) buffer
    handleBoundary (InsertChunk chunkSiz) n k buffer = insertChunk chunkSiz n (k ()) buffer
    handleBoundary (OneShotAction action) n k buffer = oneShotAction action n (k ()) buffer

--------------------------------------------------------------------------------
--
-- Handle chunk boundary

doubleBuffer :: Int -> BuildStep s -> BuildStep s
doubleBuffer !wantSiz k buffer@(Buffer buf offset) = do
    !siz <- A.sizeofMutableArr buf
    let !siz' = max (offset + wantSiz `shiftL` 1)
                    (siz `shiftL` 1)
    buf' <- A.resizeMutableArr buf siz'   -- double the buffer
    k (Buffer buf' offset)                 -- continue building
{-# INLINE doubleBuffer #-}

insertChunk :: Int -> Int -> BuildStep s -> BuildStep s
{-# INLINE insertChunk #-}
insertChunk !chunkSiz !wantSiz k buffer@(Buffer buf offset) = do
    !siz <- A.sizeofMutableArr buf
    case () of
        _
            | offset /= 0 -> do     -- this is certainly hold, but we still guard it
                when (offset < siz)
                    (A.shrinkMutableArr buf offset)            -- shrink old buffer if not full
                arr <- A.unsafeFreezeArr buf                   -- popup old buffer
                buf' <- A.newArr (max wantSiz chunkSiz)        -- make a new buffer
                xs <- unsafeInterleaveST (k (Buffer buf' 0))  -- delay the rest building process
                let v = V.fromArr arr 0 offset
                v `seq` pure (v : xs)
            | wantSiz <= siz -> k (Buffer buf 0)
            | otherwise -> do
                buf' <- A.newArr wantSiz        -- make a new buffer
                k (Buffer buf' 0 )

oneShotAction :: (V.Bytes -> ST s ()) -> Int -> BuildStep s -> BuildStep s
{-# INLINE oneShotAction #-}
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

--------------------------------------------------------------------------------

-- | shortcut to 'buildBytesWith' 'V.defaultInitSize'.
buildBytes :: Builder a -> V.Bytes
{-# INLINE buildBytes #-}
buildBytes = buildBytesWith V.defaultInitSize

-- | run Builder with 'DoubleBuffer' strategy, which is suitable
-- for building short bytes.
buildBytesWith :: Int -> Builder a -> V.Bytes
{-# INLINABLE buildBytesWith #-}
buildBytesWith initSiz (Builder b) = runST $ do
    buf <- A.newArr initSiz
    [bs] <- b DoubleBuffer lastStep (Buffer buf 0 )
    pure bs
  where
    lastStep _ (Buffer buf offset) = do
        siz <- A.sizeofMutableArr buf
        when (offset < siz) (A.shrinkMutableArr buf offset)
        arr <- A.unsafeFreezeArr buf
        pure [V.PrimVector arr 0 offset]

-- | shortcut to 'buildBytesListWith' 'V.defaultChunkSize'.
buildBytesList :: Builder a -> [V.Bytes]
{-# INLINE buildBytesList #-}
buildBytesList = buildBytesListWith  V.smallChunkSize V.defaultChunkSize

-- | run Builder with 'InsertChunk' strategy, which is suitable
-- for building lazy bytes chunks.
buildBytesListWith :: Int -> Int -> Builder a -> [V.Bytes]
{-# INLINABLE buildBytesListWith #-}
buildBytesListWith initSiz chunkSiz (Builder b) = runST $ do
    buf <- A.newArr initSiz
    b (InsertChunk chunkSiz) lastStep (Buffer buf 0)
  where
    lastStep _ (Buffer buf offset) = do
        arr <- A.unsafeFreezeArr buf
        pure [V.PrimVector arr 0 offset]

-- | shortcut to 'buildAndRunWith' 'V.defaultChunkSize'.
buildAndRun :: (V.Bytes -> IO ()) -> Builder a -> IO ()
buildAndRun = buildAndRunWith V.defaultChunkSize

-- | run Builder with 'OneShotAction' strategy, which is suitable
-- for doing effects while building.
buildAndRunWith :: Int -> (V.Bytes -> IO ()) -> Builder a -> IO ()
buildAndRunWith chunkSiz action (Builder b) = do
    buf <- A.newArr chunkSiz
    _ <- stToIO (b (OneShotAction (\ bs -> ioToPrim (action bs))) lastStep (Buffer buf 0))
    pure ()
  where
    lastStep :: a -> BuildStep RealWorld
    lastStep _ (Buffer buf offset) = do
        arr <- A.unsafeFreezeArr buf
        ioToPrim (action (V.PrimVector arr 0 offset))
        pure [] -- to match the silly pure type
{-# INLINABLE buildAndRun #-}

--------------------------------------------------------------------------------

atMost :: Int  -- ^ size bound
       -> (forall s. A.MutablePrimArray s Word8 -> Int -> ST s Int)  -- ^ the writer which pure a new offset
                                                                       -- for next write
       -> Builder ()
{-# INLINE atMost #-}
atMost n f = ensureN n `append`
    Builder (\ _  k (Buffer buf offset ) ->
        f buf offset >>= \ offset' -> k () (Buffer buf offset'))

writeN :: Int  -- ^ size bound
       -> (forall s. A.MutablePrimArray s Word8 -> Int -> ST s ())  -- ^ the writer which pure a new offset
                                                                    -- for next write
       -> Builder ()
{-# INLINE writeN #-}
writeN n f = ensureN n `append`
    Builder (\ _  k (Buffer buf offset ) ->
        f buf offset >> k () (Buffer buf (offset+n)))

-- | write primitive types in host byte order.
encodePrim :: forall a. UnalignedAccess a => a -> Builder ()
{-# INLINE encodePrim #-}
{-# SPECIALIZE INLINE encodePrim :: Word -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Word64 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Word32 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Word16 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Word8 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Int -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Int64 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Int32 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Int16 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Int8 -> Builder () #-}
encodePrim x = do
    ensureN n
    Builder (\ _  k (Buffer (MutablePrimArray mba#) i@(I# i#)) -> do
        primitive_ (writeWord8ArrayAs mba# i# x)
        k () (Buffer (MutablePrimArray mba#) (i + n)))
  where
    n = (getUnalignedSize (unalignedSize :: UnalignedSize a))

-- | write primitive types with little endianess.
encodePrimLE :: forall a. UnalignedAccess (LE a) => a -> Builder ()
{-# INLINE encodePrimLE #-}
{-# SPECIALIZE INLINE encodePrimLE :: Word -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Word64 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Word32 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Word16 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Int -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Int64 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Int32 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Int16 -> Builder () #-}
encodePrimLE = encodePrim . LE

-- | write primitive types with big endianess.
encodePrimBE :: forall a. UnalignedAccess (BE a) => a -> Builder ()
{-# INLINE encodePrimBE #-}
{-# SPECIALIZE INLINE encodePrimBE :: Word -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Word64 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Word32 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Word16 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Int -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Int64 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Int32 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Int16 -> Builder () #-}
encodePrimBE = encodePrim . BE

--------------------------------------------------------------------------------

-- | Turn 'String' into 'Builder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's.
--
-- Note, if you're trying to write string literals builders, and you know it doen't contain
-- '\NUL' or surrgate codepoints, then you can open 'OverloadedStrings' and use 'Builder''s
-- 'IsString' instance, it can save an extra UTF-8 validation.
--
-- This function will be rewritten into a memcpy if possible, (running a fast UTF-8 validation
-- at runtime first).
stringUTF8 :: String -> Builder ()
{-# INLINE CONLIKE [0] stringUTF8 #-}
{-# RULES
    "stringUTF8/packASCIIAddr" forall addr . stringUTF8 (unpackCString# addr) = packASCIIAddr addr
  #-}
{-# RULES
    "stringUTF8/packUTF8Addr" forall addr . stringUTF8 (unpackCString# addr) = packUTF8Addr addr
  #-}
stringUTF8 = mapM_ charUTF8

packASCIIAddr :: Addr# -> Builder ()
packASCIIAddr addr# = copy addr#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr#
    copy addr# = do
        ensureN len
        Builder (\ _  k (Buffer mba i) -> do
           copyPtrToMutablePrimArray mba i (Ptr addr#) len
           k () (Buffer mba (i + len)))

packUTF8Addr :: Addr# -> Builder ()
packUTF8Addr addr# = validateAndCopy addr#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr#
    valid = unsafeDupablePerformIO $ T.c_utf8_validate_addr addr# len
    validateAndCopy addr#
        | valid == 0 = mapM_ charUTF8 (unpackCString# addr#)
        | otherwise = do
            ensureN len
            Builder (\ _  k (Buffer mba i) -> do
               copyPtrToMutablePrimArray mba i (Ptr addr#) len
               k () (Buffer mba (i + len)))

-- | Turn 'Char' into 'Builder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's.
charUTF8 :: Char -> Builder ()
{-# INLINE charUTF8 #-}
charUTF8 chr = do
    ensureN 4
    Builder (\ _  k (Buffer mba i) -> do
        i' <- T.encodeChar mba i chr
        k () (Buffer mba i'))

-- | Turn 'String' into 'Builder' with ASCII7 encoding
--
-- Codepoints beyond @'\x7F'@ will be chopped.
string7 :: String -> Builder ()
{-# INLINE string7 #-}
string7 = mapM_ char7

-- | Turn 'Char' into 'Builder' with ASCII7 encoding
--
-- Codepoints beyond @'\x7F'@ will be chopped.
char7 :: Char -> Builder ()
{-# INLINE char7 #-}
char7 chr = do
    ensureN 1
    Builder (\ _  k (Buffer mba@(MutablePrimArray mba#) i@(I# i#)) -> do
        let x = V.c2w chr .&. 0x7F
        primitive_ (writeWord8ArrayAs mba# i# x)
        k () (Buffer mba (i+1)))

-- | Turn 'String' into 'Builder' with ASCII8 encoding
--
-- Codepoints beyond @'\xFF'@ will be chopped.
-- Note, this encoding is NOT compatible with UTF8 encoding, i.e. bytes written
-- by this builder may not be legal UTF8 encoding bytes.
string8 :: String -> Builder ()
{-# INLINE string8 #-}
string8 = mapM_ char8

-- | Turn 'Char' into 'Builder' with ASCII8 encoding
--
-- Codepoints beyond @'\xFF'@ will be chopped.
-- Note, this encoding is NOT compatible with UTF8 encoding, i.e. bytes written
-- by this builder may not be legal UTF8 encoding bytes.
char8 :: Char -> Builder ()
{-# INLINE char8 #-}
char8 chr = do
    ensureN 1
    Builder (\ _  k (Buffer mba@(MutablePrimArray mba#) i@(I# i#)) -> do
        let x = V.c2w chr
        primitive_ (writeWord8ArrayAs mba# i# x)
        k () (Buffer mba (i+1)))

-- | Write UTF8 encoded 'Text' using 'Builder'.
--
-- Note, if you're trying to write string literals builders,
-- please open 'OverloadedStrings' and use 'Builder's 'IsString' instance,
-- it will be rewritten into a memcpy.
text :: T.Text -> Builder ()
{-# INLINE text #-}
text (T.Text bs) = bytes bs
