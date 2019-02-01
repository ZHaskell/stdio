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
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
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

module Std.Data.Builder.Base where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe            (unsafeInterleaveST)
import           Data.Bits                          (shiftL, shiftR)
import           Data.Bits                          ((.&.))
import           Data.Monoid                        (Monoid (..))
import           Data.Primitive.PrimArray           (MutablePrimArray (..))
import           Data.Primitive.Ptr                 (copyPtrToMutablePrimArray)
import           Data.Semigroup                     (Semigroup (..))
import           Data.String                        (IsString (..))
import           Data.Word
import           GHC.CString                        (unpackCString#)
import           GHC.Prim
import           GHC.Ptr
import           GHC.Types
import qualified Std.Data.Array                     as A
import           Std.Data.PrimArray.UnalignedAccess
import qualified Std.Data.Text.Base                 as T
import qualified Std.Data.Text.UTF8Codec            as T
import qualified Std.Data.Vector.Base               as V
import           System.IO.Unsafe

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
data Buffer s = Buffer {-# UNPACK #-} !(A.MutablePrimArray s Word8)  -- well, the buffer content
                       {-# UNPACK #-} !Int  -- writing offset

-- | @BuilderStep@ is a function that fill buffer under given conditions.
--
type BuildStep s = Buffer s -> ST s [V.Bytes]

-- | @Builder@ is a monoid to help compose @BuilderStep@. With next @BuilderStep@ continuation,
-- We can do interesting things like perform some action, or interleave the build process.
--
newtype Builder a = Builder
    { runBuilder :: forall s. AllocateStrategy s -> (a -> BuildStep s) -> BuildStep s}

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
    fromString = stringLiteral

-- | A alias to 'stringUTF8', but will be rewritten to a memcpy if possible.
stringLiteral :: String -> Builder ()
{-# NOINLINE CONLIKE stringLiteral #-}
{-# RULES
    "stringLiteral/addrLiteral" forall addr . stringLiteral (unpackCString# addr) = addrLiteral addr
  #-}
stringLiteral = stringUTF8

addrLiteral :: Addr# -> Builder ()
addrLiteral addr# = validateAndCopy addr#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr#
    valid = unsafeDupablePerformIO $ T.c_utf8_validate addr# 0 len
    validateAndCopy addr#
        | valid == 0 = stringLiteral (unpackCString# addr#)
        | otherwise = do
            ensureN len
            Builder (\ _  k (Buffer mba i) -> do
               copyPtrToMutablePrimArray mba i (Ptr addr#) len
               k () (Buffer mba (i + len)))


append :: Builder a -> Builder b -> Builder b
append (Builder f) (Builder g) = Builder (\ al k -> f al ( \ _ ->  g al k))
{-# INLINE append #-}


--------------------------------------------------------------------------------

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
-- | Handle chunk boundary
--
doubleBuffer :: Int -> BuildStep s -> BuildStep s
doubleBuffer !wantSiz k buffer@(Buffer buf offset) = do
    !siz <- A.sizeofMutableArr buf
    let !siz' = max (offset + wantSiz `shiftL` 1)
                    (siz `shiftL` 1)
    buf' <- A.resizeMutableArr buf siz'   -- double the buffer
    k (Buffer buf' offset)                 -- continue building
{-# INLINE doubleBuffer #-}

insertChunk :: Int -> Int -> BuildStep s -> BuildStep s
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
                v `seq` return (v : xs)
            | wantSiz <= siz -> k (Buffer buf 0)
            | otherwise -> do
                buf' <- A.newArr wantSiz        -- make a new buffer
                k (Buffer buf' 0 )
{-# INLINE insertChunk #-}

oneShotAction :: (V.Bytes -> ST s ()) -> Int -> BuildStep s -> BuildStep s
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

buildBytes :: Builder a -> V.Bytes
buildBytes = buildBytesWith V.defaultInitSize

buildBytesWith :: Int -> Builder a -> V.Bytes
buildBytesWith initSiz (Builder b) = runST $ do
    buf <- A.newArr initSiz
    [bs] <- b DoubleBuffer lastStep (Buffer buf 0 )
    return bs
  where
    lastStep _ (Buffer buf offset) = do
        siz <- A.sizeofMutableArr buf
        when (offset < siz) (A.shrinkMutableArr buf offset)
        arr <- A.unsafeFreezeArr buf
        return [V.PrimVector arr 0 offset]
{-# INLINABLE buildBytes #-}


buildBytesList :: Builder a -> [V.Bytes]
buildBytesList = buildBytesListWith  V.smallChunkSize V.defaultChunkSize

buildBytesListWith :: Int -> Int -> Builder a -> [V.Bytes]
buildBytesListWith initSiz chunkSiz (Builder b) = runST $ do
    buf <- A.newArr initSiz
    b (InsertChunk chunkSiz) lastStep (Buffer buf 0)
  where
    lastStep _ (Buffer buf offset) = do
        arr <- A.unsafeFreezeArr buf
        return [V.PrimVector arr 0 offset]
{-# INLINABLE buildBytesList #-}

buildAndRun :: (V.Bytes -> IO ()) -> Builder a -> IO ()
buildAndRun = buildAndRunWith V.defaultChunkSize

buildAndRunWith :: Int -> (V.Bytes -> IO ()) -> Builder a -> IO ()
buildAndRunWith chunkSiz action (Builder b) = do
    buf <- A.newArr chunkSiz
    _ <- stToIO (b (OneShotAction (\ bs -> ioToPrim (action bs))) lastStep (Buffer buf 0))
    return ()
  where
    lastStep :: a -> BuildStep RealWorld
    lastStep _ (Buffer buf offset) = do
        arr <- A.unsafeFreezeArr buf
        ioToPrim (action (V.PrimVector arr 0 offset))
        return [] -- to match the silly return type
{-# INLINABLE buildAndRun #-}

--------------------------------------------------------------------------------

atMost :: Int  -- ^ size bound
       -> (forall s. A.MutablePrimArray s Word8 -> Int -> ST s Int)  -- ^ the writer which return a new offset
                                                                       -- for next write
       -> Builder ()
{-# INLINE atMost #-}
atMost n f = ensureN n `append`
    Builder (\ _  k (Buffer buf offset ) ->
        f buf offset >>= \ offset' -> k () (Buffer buf offset'))

writeN :: Int  -- ^ size bound
       -> (forall s. A.MutablePrimArray s Word8 -> Int -> ST s ())  -- ^ the writer which return a new offset
                                                                    -- for next write
       -> Builder ()
{-# INLINE writeN #-}
writeN n f = ensureN n `append`
    Builder (\ _  k (Buffer buf offset ) ->
        f buf offset >> k () (Buffer buf (offset+n)))

encodePrim :: forall a. UnalignedAccess a => a -> Builder ()
{-# INLINE encodePrim #-}
encodePrim x = do
    ensureN n
    Builder (\ _  k (Buffer (MutablePrimArray mba#) i@(I# i#)) -> do
        primitive_ (writeWord8ArrayAs mba# i# x)
        k () (Buffer (MutablePrimArray mba#) (i + n)))
  where
    n = (getUnalignedSize (unalignedSize :: UnalignedSize a))

encodePrimLE :: forall a. UnalignedAccess (LE a) => a -> Builder ()
{-# INLINE encodePrimLE #-}
encodePrimLE = encodePrim . LE

encodePrimBE :: forall a. UnalignedAccess (BE a) => a -> Builder ()
{-# INLINE encodePrimBE #-}
encodePrimBE = encodePrim . BE

--------------------------------------------------------------------------------

-- | Turn 'String' into 'Builder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's.
--
-- Note, if you're trying to write string literals builders,
-- please open 'OverloadedStrings' and use 'Builder's 'IsString' instance,
-- it will use 'stringLiteral' and will be rewritten into a memcpy,
-- instead of encoding 'Char's in a loop like what 'stringUTF8' do.
stringUTF8 :: String -> Builder ()
{-# INLINE stringUTF8 #-}
stringUTF8 = mapM_ charUTF8

-- | Turn 'Char' into 'Builder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's.
charUTF8 :: Char -> Builder ()
{-# INLINE charUTF8 #-}
charUTF8 chr = do
    let !n = T.encodeCharLength chr
    ensureN n
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
-- it will use 'stringLiteral' and will be rewritten into a memcpy.
text :: T.Text -> Builder ()
{-# INLINE text #-}
text (T.Text bs) = bytes bs
