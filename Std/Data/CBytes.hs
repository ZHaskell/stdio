{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE BangPatterns #-}
{-|
Module      : Std.Data.CBytes
Description : Null-ternimated byte string.
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide 'CBytes' with some useful instances \/ functions, A 'CBytes' is a
wrapper for immutable null-terminated string.
The main design target of this type is to ease the bridging of C FFI APIs, since most
of the unix APIs use null-terminated string. On windows you're encouraged to use a
compatibility layer like 'WideCharToMultiByte/MultiByteToWideChar' and keep the same
interface, e.g. libuv do this when deal with file paths.

We neither guarantee to store length info, nor support O(1) slice for 'CBytes':
This will defeat the purpose of null-terminated string which is to save memory,
We do save the length if it's created on GHC heap though. If you need advance editing,
convert a 'CBytes' to 'V.Bytes' with 'toBytes' and use vector combinators.
Use 'fromBytes' to convert it back.

It can be used with @OverloadedString@, literal encoding is UTF-8 with some modifications:
@\NUL@ char is encoded to 'C0 80', and '\xD800' ~ '\xDFFF' is encoded as a three bytes
normal utf-8 codepoint. This is also how ghc compile string literal into binaries,
thus we can use rewrite-rules to construct 'CBytes' value in O(1) without wasting runtime heap.

Note most of the unix API is not unicode awared though, you may find a `scandir` call
return a filename which is not proper encoded in any unicode encoding at all.
But still, UTF-8 is recommanded to be used everywhere, and we use UTF-8 assumption in
various places, such as displaying 'CBytes' and literals encoding above.

-}

module Std.Data.CBytes
  ( CBytes
  , create
  , pack
  , unpack
  , null , length
  , empty, append, concat
  , toBytes, fromBytes
  , fromCStringMaybe, fromCString
  , withCBytes
  -- * Misc
  , c_strlen
  , c_strcmp
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits
import           Data.Foldable           (foldlM)
import qualified Data.List               as List
import           Data.Monoid             (Monoid (..))
import           Data.Semigroup          (Semigroup (..))
import           Data.String             (IsString (..))
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           Data.Word
import           Foreign.C
import           Foreign.Storable        (peekElemOff)
import           GHC.CString
import           GHC.Ptr
import           Prelude                 hiding (all, any, appendFile, break,
                                          concat, concatMap, drop, dropWhile,
                                          elem, filter, foldl, foldl1, foldr,
                                          foldr1, getContents, getLine, head,
                                          init, interact, last, length, lines,
                                          map, maximum, minimum, notElem, null,
                                          putStr, putStrLn, readFile, replicate,
                                          reverse, scanl, scanl1, scanr, scanr1,
                                          span, splitAt, tail, take, takeWhile,
                                          unlines, unzip, writeFile, zip,
                                          zipWith)
import           Std.Data.Array
import           Std.Data.Text.UTF8Codec (encodeCBytesChar)
import qualified Std.Data.Vector.Base    as V
import           Std.IO.Exception
import           System.IO.Unsafe        (unsafeDupablePerformIO)

-- | A efficient wrapper for immutable null-terminated string which can be
-- automatically freed by ghc garbage collector.
--
data CBytes
    = CBytesOnHeap  {-# UNPACK #-} !(PrimArray Word8)   -- ^ On heap pinned 'PrimArray'
    | CBytesLiteral {-# UNPACK #-} !CString             -- ^ String literals with static address

-- | Create a 'CBytes' with IO action.
--
-- Note: The initial content of the 'CString' is random, it doesn't even have
-- a proper '\NUL' ternimator, user should take the responsibility to do a
-- proper initialization and return the actual length.
--
create :: HasCallStack
       => Int  -- capacity, includeing the '\NUL' terminator
       -> (CString -> IO Int)  -- initialization function,
                               -- write the pointer, return the length
       -> IO CBytes
{-# INLINE create #-}
create n fill = do
    mba <- newPinnedPrimArray n :: IO (MutablePrimArray RealWorld Word8)
    l <- withMutablePrimArrayContents mba (fill . castPtr)
    writePrimArray mba l 0 -- the '\NUL' ternimator
    shrinkMutablePrimArray mba (l+1)
    CBytesOnHeap <$> unsafeFreezePrimArray mba

instance Show CBytes where
    show = unpack

instance Read CBytes where
    readsPrec p s = [(pack x, r) | (x, r) <- readsPrec p s]

instance Eq CBytes where
    cbyteA == cbyteB = unsafeDupablePerformIO $
        withCBytes cbyteA $ \ pA ->
        withCBytes cbyteB $ \ pB ->
            if pA == pB
            then return True
            else do
                r <- c_strcmp pA pB
                return (r == 0)

instance Ord CBytes where
    cbyteA `compare` cbyteB = unsafeDupablePerformIO $
        withCBytes cbyteA $ \ pA ->
        withCBytes cbyteB $ \ pB ->
            if pA == pB
            then return EQ
            else do
                r <- c_strcmp pA pB
                return (r `compare` 0)

instance Semigroup CBytes where
    (<>) = append

instance Monoid CBytes where
    {-# INLINE mempty #-}
    mempty  = empty
    {-# INLINE mappend #-}
    mappend = append
    {-# INLINE mconcat #-}
    mconcat = concat

append :: CBytes -> CBytes -> CBytes
{-# INLINABLE append #-}
append strA strB
    | lenA == 0 = strB
    | lenB == 0 = strA
    | otherwise = unsafeDupablePerformIO $ do
        mpa <- newPinnedPrimArray (lenA+lenB+1)
        withCBytes strA $ \ pa ->
            withCBytes strB $ \ pb -> do
                copyPtrToMutablePrimArray mpa 0    (castPtr pa) lenA
                copyPtrToMutablePrimArray mpa lenA (castPtr pb) lenB
                writePrimArray mpa (lenA + lenB) 0     -- the \NUL terminator
                pa <- unsafeFreezePrimArray mpa
                return (CBytesOnHeap pa)
  where
    lenA = length strA
    lenB = length strB

empty :: CBytes
{-# NOINLINE empty #-}
empty = CBytesLiteral (Ptr "\0"#)

concat :: [CBytes] -> CBytes
{-# INLINABLE concat #-}
concat bs = case pre 0 0 bs of
    (0, _) -> empty
    (1, _) -> let Just b = List.find (not . null) bs in b -- there must be a not empty CBytes
    (_, l) -> runST $ do
        buf <- newPinnedPrimArray (l+1)
        copy bs 0 buf
        writePrimArray buf l 0 -- the \NUL terminator
        CBytesOnHeap <$> unsafeFreezePrimArray buf
  where
    -- pre scan to decide if we really need to copy and calculate total length
    -- we don't accumulate another result list, since it's rare to got empty
    pre :: Int -> Int -> [CBytes] -> (Int, Int)
    pre !nacc !lacc [] = (nacc, lacc)
    pre !nacc !lacc (b:bs)
        | length b <= 0 = pre nacc lacc bs
        | otherwise     = pre (nacc+1) (length b + lacc) bs

    copy :: [CBytes] -> Int -> MutablePrimArray s Word8 -> ST s ()
    copy [] !_ !_       = return ()
    copy (b:bs) !i !mba = do
        let l = length b
        when (l /= 0) (case b of
            CBytesOnHeap ba ->
                copyPrimArray mba i ba 0 l
            CBytesLiteral p ->
                copyPtrToMutablePrimArray mba i (castPtr p) l)
        copy bs (i+l) mba

instance IsString CBytes where
    {-# INLINE fromString #-}
    fromString = pack

{-# RULES
    "CBytes pack/unpackCString#" forall addr# .
        pack (unpackCString# addr#) = CBytesLiteral (Ptr addr#)
 #-}
{-# RULES
    "CBytes pack/unpackCStringUtf8#" forall addr# .
        pack (unpackCStringUtf8# addr#) = CBytesLiteral (Ptr addr#)
 #-}

-- | Pack a 'String' into null-terminated 'CBytes'.
--
pack :: String -> CBytes
{-# NOINLINE [1] pack #-}
pack s = runST $ do
    mba <- newPinnedPrimArray V.defaultInitSize
    (SP2 i mba') <- foldlM go (SP2 0 mba) s
    writePrimArray mba' i 0     -- the \NUL terminator
    shrinkMutablePrimArray mba' (i+1)
    ba <- unsafeFreezePrimArray mba'
    return (CBytesOnHeap ba)
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: SP2 s -> Char -> ST s (SP2 s)
    go sp          '\NUL' = return sp
    go (SP2 i mba) !c     = do
        siz <- getSizeofMutablePrimArray mba
        if i < siz - 4  -- we need at least 5 bytes for safety due to extra '\0' byte
        then do
            i' <- encodeCBytesChar mba i c
            return (SP2 i' mba)
        else do
            let !siz' = siz `shiftL` 1
            !mba' <- resizeMutablePrimArray mba siz'
            i' <- encodeCBytesChar mba' i c
            return (SP2 i' mba')


data SP2 s = SP2 {-# UNPACK #-}!Int {-# UNPACK #-}!(MutablePrimArray s Word8)

unpack :: CBytes -> String
{-# INLINABLE unpack #-}
-- TODO: rewrite with our own decoder
unpack cbytes = unsafeDupablePerformIO . withCBytes cbytes $ \ (Ptr addr#) ->
    return (unpackCStringUtf8# addr#)

--------------------------------------------------------------------------------

null :: CBytes -> Bool
{-# INLINE null #-}
null (CBytesOnHeap pa) = indexArr pa 0 == 0
null (CBytesLiteral p) = unsafeDupablePerformIO (peekElemOff p 0) == 0

length :: CBytes -> Int
{-# INLINE length #-}
length (CBytesOnHeap pa) = sizeofArr pa - 1
length (CBytesLiteral p) = fromIntegral $ unsafeDupablePerformIO (c_strlen p)

-- | /O(1)/, (/O(n)/ in case of literal), convert to 'V.Bytes', which can be
-- processed by vector combinators.
--
-- NOTE: the '\NUL' ternimator is not included.
toBytes :: CBytes -> V.Bytes
{-# INLINABLE toBytes #-}
toBytes cbytes@(CBytesOnHeap pa) = V.fromArr pa 0 l
  where l = length cbytes
toBytes cbytes@(CBytesLiteral p) = V.create (l+1) (\ mpa -> do
    copyPtrToMutablePrimArray mpa 0 (castPtr p) l
    writePrimArray mpa l 0)    -- the \NUL terminator
  where l = length cbytes

-- | /O(n)/, convert from 'V.Bytes', allocate pinned memory and
-- add the '\NUL' ternimator
fromBytes :: V.Bytes -> CBytes
{-# INLINABLE fromBytes #-}
fromBytes (V.Vec arr s l) =  runST (do
        mpa <- newPinnedPrimArray (l+1)
        copyPrimArray mpa 0 arr s l
        writePrimArray mpa l 0     -- the \NUL terminator
        pa <- unsafeFreezePrimArray mpa
        return (CBytesOnHeap pa))

--------------------------------------------------------------------------------

-- | Copy a 'CString' type into a 'CBytes', return Nothing if the pointer is NULL.
--
--  After copying you're free to free the 'CString' 's memory.
--
fromCStringMaybe :: HasCallStack => CString -> IO (Maybe CBytes)
{-# INLINABLE fromCStringMaybe #-}
fromCStringMaybe cstring =
    if cstring == nullPtr
    then return Nothing
    else do
        len <- fromIntegral <$> c_strlen cstring
        mpa <- newPinnedPrimArray (len+1)
        copyPtrToMutablePrimArray mpa 0 (castPtr cstring) len
        writePrimArray mpa len 0     -- the \NUL terminator
        pa <- unsafeFreezePrimArray mpa
        return (Just (CBytesOnHeap pa))


-- | Same with 'fromCString', but throw 'InvalidArgument' when meet a null pointer.
--
fromCString :: HasCallStack
            => CString
            -> IO CBytes
{-# INLINABLE fromCString #-}
fromCString cstring =
    if cstring == nullPtr
    then throwIO (InvalidArgument
        (IOEInfo "" "unexpected null pointer" callStack))
    else do
        len <- fromIntegral <$> c_strlen cstring
        mpa <- newPinnedPrimArray (len+1)
        copyPtrToMutablePrimArray mpa 0 (castPtr cstring) len
        writePrimArray mpa len 0     -- the \NUL terminator
        pa <- unsafeFreezePrimArray mpa
        return (CBytesOnHeap pa)

-- | Pass 'CBytes' to foreign function as a @const char*@.
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withCBytes :: CBytes -> (CString -> IO a) -> IO a
{-# INLINABLE withCBytes #-}
withCBytes (CBytesOnHeap pa) f = withPrimArrayContents pa (f . castPtr)
withCBytes (CBytesLiteral ptr) f = f ptr

--------------------------------------------------------------------------------

foreign import ccall unsafe "string.h strcmp"
    c_strcmp :: CString -> CString -> IO CInt

foreign import ccall unsafe "string.h strlen"
    c_strlen :: CString -> IO CSize
