{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

{-|
Module      : Std.Data.Text.Base
Description : Unicode text processing
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

A 'Text' wrap a 'Bytes' which will be interpreted using UTF-8 encoding. User should always use 'validate' to construt a 'Text' (instead of using construtor directly or coercing), otherwise illegal UTF-8
encoded codepoints will cause undefined behaviours.

-}

module Std.Data.Text.Base (
  -- * Text type
    Text(..)
  , validate
  , validateMaybe
  , charAt, byteAt, charAtLast, byteAtLast
  -- * Basic creating
  , empty, singleton
  -- * Conversion between list
  , pack, packN, packR, packRN
  , unpack, unpackR
  -- * Conversion between codepoint vector
  , fromVector
  , toVector
  -- * Basic interface
  , null
  , length
  , append
  , map', imap'
  , foldl', ifoldl'
  , foldr', ifoldr'
    -- ** Case conversion
  , toCaseFold, toCaseFold', toLower, toUpper, toTitle
    -- ** Special folds
  , concat, concatMap
  , count
  , all, any
  -- * Building text
  , replicate
  , cycleN
 ) where

import           Control.DeepSeq
import           Control.Monad.ST
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Foldable            (foldlM)
import qualified Data.List                as List
import           Data.Primitive.PrimArray
import           Data.Typeable
import           Data.Word
import           Foreign.C.Types          (CSize(..))
import           GHC.Exts                 (build)
import           GHC.Prim
import           GHC.Types
import           GHC.Stack
import           Std.Data.Array
import           Std.Data.Text.UTF8Codec
import           Std.Data.Text.UTF8Rewind
import           Std.Data.Vector.Base     (Bytes, PrimVector)
import qualified Std.Data.Vector.Base     as V
import qualified Std.Data.Vector.Extra    as V
import qualified Std.Data.Vector.Search   as V
import           Std.Data.Parser   (Result(..))
import           System.IO.Unsafe (unsafeDupablePerformIO)

import           Prelude                       hiding (concat, concatMap,
                                                elem, notElem, null, length, map,
                                                foldl, foldl1, foldr, foldr1,
                                                maximum, minimum, product, sum,
                                                all, any, replicate, traverse)

-- | 'Text' represented as UTF-8 encoded 'Bytes'
--
newtype Text = Text { getUTF8Bytes :: Bytes }

instance Eq Text where
    Text b1 == Text b2 = b1 == b2
    {-# INLINE (==) #-}

instance Ord Text where
    Text b1 `compare` Text b2 = b1 `compare` b2 -- UTF-8 encoding property
    {-# INLINE compare #-}

instance Show Text where
    showsPrec p t = showsPrec p (unpack t)
instance Read Text where
    readsPrec p str = [ (pack x, y) | (x, y) <- readsPrec p str ]
instance NFData Text where
    rnf (Text bs) = rnf bs

-- | /O(n)/ Get the nth codepoint from 'Text'.
charAt :: Text -> Int -> Maybe Char
{-# INLINABLE charAt #-}
charAt (Text (V.PrimVector ba s l)) n
    | n < 0 = Nothing
    | otherwise = go s 0
  where
    !end = s + l
    go !i !j
        | i >= end = Nothing
        | j >= n = let !c = decodeChar_ ba i in Just c
        | otherwise =
            let l = decodeCharLen ba i in go (i+l) (j+1)

-- | /O(n)/ Find the nth codepoint's byte index (pointing to the nth char's begining byte).
--
-- The index is only meaningful to the whole byte slice, if there's less than n codepoints,
-- the index will point to next byte after the end.
byteAt :: Text -> Int -> Int
{-# INLINABLE byteAt #-}
byteAt (Text (V.PrimVector ba s l)) n
    | n < 0 = s
    | otherwise = go s 0
  where
    !end = s + l
    go !i !j
        | i >= end = i
        | j >= n = i
        | otherwise =
            let l = decodeCharLen ba i in go (i+l) (j+1)

-- | /O(n)/ Get the nth codepoint from 'Text' counting from the end.
charAtLast :: Text -> Int -> Maybe Char
{-# INLINABLE charAtLast #-}
charAtLast (Text (V.PrimVector ba s l)) n
    | n < 0 = Nothing
    | otherwise = go (s+l-1) 0
  where
    go !i !j
        | i < s = Nothing
        | j >= n = let !c = decodeCharReverse_ ba i in Just c
        | otherwise =
            let l = decodeCharLenReverse ba i in go (i-l) (j+1)

-- | /O(n)/ Find the nth codepoint's byte index from the end
-- (pointing to the previous char's ending byte).
--
-- The index is only meaningful to the whole byte slice, if there's less than n codepoints,
-- the index will point to previous byte before the start.
byteAtLast :: Text -> Int -> Int
{-# INLINABLE byteAtLast #-}
byteAtLast (Text (V.PrimVector ba s l)) n
    | n < 0 = s+l
    | otherwise = go (s+l-1) 0
  where
    go !i !j
        | i < s = i
        | j >= n = i
        | otherwise =
            let l = decodeCharLenReverse ba i in go (i-l) (j+1)

--------------------------------------------------------------------------------

-- | /O(n)/ Validate a sequence of bytes is UTF-8 encoded.
--
-- Throw error in case of invalid codepoint.
--
validate :: HasCallStack => Bytes -> Text
{-# INLINE validate #-}
validate bs@(V.PrimVector (PrimArray ba#) (I# s#) l@(I# l#))
    | l == 0 = Text bs
    | utf8_validate ba# s# l# > 0 = Text bs
    | otherwise = error "invalid UTF8 bytes"

validateMaybe :: Bytes -> Maybe Text
{-# INLINE validateMaybe #-}
validateMaybe bs@(V.PrimVector (PrimArray ba#) (I# s#) l@(I# l#))
    | l == 0 = Just (Text bs)
    | utf8_validate ba# s# l# > 0 = Just (Text bs)
    | otherwise = Nothing

foreign import ccall unsafe utf8_validate :: ByteArray# -> Int# -> Int# -> Int

--------------------------------------------------------------------------------

pack :: String -> Text
pack = packN V.defaultInitSize
{-# INLINE pack #-}

-- | /O(n)/ Convert a list into a text with an approximate size(in bytes, not codepoints).
--
-- If the encoded bytes length is larger than the size given, we simply double the buffer size
-- and continue building.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packN :: Int -> String -> Text
{-# INLINE packN #-}
packN n0 = \ ws0 ->
    Text (V.create' (max 4 n0) (\ marr -> foldlM go (V.IPair 0 marr) ws0))
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: V.IPair (MutablePrimArray s Word8) -> Char -> ST s (V.IPair (MutablePrimArray s Word8))
    go (V.IPair i marr) !c = do
        siz <- getSizeofMutablePrimArray marr
        if i < siz - 3  -- we need at least 4 bytes for safety
        then do
            i' <- encodeChar marr i c
            return (V.IPair i' marr)
        else do
            let !siz' = siz `shiftL` 1
            !marr' <- resizeMutablePrimArray marr siz'
            i' <- encodeChar marr' i c
            return (V.IPair i' marr')

-- | /O(n)/
--
-- Alias for @'packRN' 'defaultInitSize'@.
--
packR :: String -> Text
{-# INLINE packR #-}
packR = packRN V.defaultInitSize

-- | /O(n)/ 'packN' in reverse order.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packRN :: Int -> String -> Text
{-# INLINE packRN #-}
packRN n0 = \ ws0 -> runST (do let n = max 4 n0
                               marr <- newArr n
                               (V.IPair i marr') <- foldM go (V.IPair n marr) ws0
                               ba <- unsafeFreezeArr marr'
                               return $! Text (V.fromArr ba i (sizeofArr ba-i))
                           )
  where
    go :: V.IPair (MutablePrimArray s Word8) -> Char -> ST s (V.IPair (MutablePrimArray s Word8))
    go (V.IPair i marr) !c = do
        n <- sizeofMutableArr marr
        let l = encodeCharLength c
        if i >= l
        then do encodeChar marr (i-l) c
                return (V.IPair (i-l) marr)
        else do let !n' = n `shiftL` 1  -- double the buffer
                !marr' <- newArr n'
                copyMutableArr marr' (n+i) marr i (n-i)
                let i' = n+i-l
                encodeChar marr' i' c
                return (V.IPair i' marr')

unpack :: Text -> String
{-# INLINE [1] unpack #-}
unpack (Text (V.PrimVector ba s l)) = go s
  where
    !end = s + l
    go !idx
        | idx >= end = []
        | otherwise = let (# c, i #) = decodeChar ba idx in c : go (idx + i)

unpackFB :: Text -> (Char -> a -> a) -> a -> a
{-# INLINE [0] unpackFB #-}
unpackFB (Text (V.PrimVector ba s l)) k z = go s
  where
    !end = s + l
    go !idx
        | idx >= end = z
        | otherwise = let (# c, i #) = decodeChar ba idx in c `k` go (idx + i)

{-# RULES
"unpack" [~1] forall t . unpack t = build (\ k z -> unpackFB t k z)
"unpackFB" [1] forall t . unpackFB t (:) [] = unpack t
 #-}

unpackR :: Text -> String
{-# INLINE [1] unpackR #-}
unpackR (Text (V.PrimVector ba s l)) = go (s+l-1)
  where
    go !idx
        | idx < s = []
        | otherwise = let (# c, i #) = decodeCharReverse ba idx in c : go (idx - i)

unpackRFB :: Text -> (Char -> a -> a) -> a -> a
{-# INLINE [0] unpackRFB #-}
unpackRFB (Text (V.PrimVector ba s l)) k z = go (s+l-1)
  where
    go !idx
        | idx < s = z
        | otherwise = let (# c, i #) = decodeCharReverse ba idx in c `k` go (idx - i)

{-# RULES
"unpackR" [~1] forall t . unpackR t = build (\ k z -> unpackRFB t k z)
"unpackRFB" [1] forall t . unpackRFB t (:) [] = unpackR t
 #-}

singleton :: Char -> Text
{-# INLINABLE singleton #-}
singleton c = Text $ V.createN 4 $ \ marr -> encodeChar marr 0 c

empty :: Text
{-# INLINABLE empty #-}
empty = Text V.empty

--------------------------------------------------------------------------------
-- * Basic interface

append :: Text -> Text -> Text
append ta tb = Text ( getUTF8Bytes ta `V.append` getUTF8Bytes tb )
{-# INLINE append #-}

null :: Text -> Bool
{-# INLINABLE null #-}
null (Text bs) = V.null bs

length :: Text -> Int
{-# INLINABLE length #-}
length (Text (V.PrimVector ba s l)) = go s 0
  where
    !end = s + l
    go !i !acc | i >= end = acc
               | otherwise = let j = decodeCharLen ba i in go (i+j) (1+acc)

--------------------------------------------------------------------------------
-- * Transformations
--
-- | /O(n)/ 'map' @f@ @t@ is the 'Text' obtained by applying @f@ to
-- each element of @t@. Performs replacement on invalid scalar values.
--
map' :: (Char -> Char) -> Text -> Text
{-# INLINE map' #-}
map' f (Text (V.PrimVector arr s l)) | l == 0 = empty
                                     | otherwise = Text (V.create' (l+3) (go s 0))
  where
    end = s + l
    -- the 3 bytes buffer is here for optimizing ascii mapping
    -- we do resize if less than 4 bytes left when building
    -- to save us from pre-checking encoding char length everytime
    go :: Int -> Int -> MutablePrimArray s Word8 -> ST s (V.IPair (MutablePrimArray s Word8))
    go !i !j !marr
        | i >= end = return (V.IPair j marr)
        | otherwise = do
            let (# c, d #) = decodeChar arr i
            j' <- encodeChar marr j (f c)
            let !i' = i + d
            siz <- sizeofMutableArr marr
            if  j' < siz - 3
            then go i' j' marr
            else do
                let !siz' = siz `shiftL` 1
                !marr' <- resizeMutablePrimArray marr siz'
                go i' j' marr'


imap' :: (Int -> Char -> Char) -> Text -> Text
{-# INLINE imap' #-}
imap' f (Text (V.PrimVector arr s l)) | l == 0 = empty
                                      | otherwise = Text (V.create' (l+3) (go s 0 0))
  where
    end = s + l
    go :: Int -> Int -> Int -> MutablePrimArray s Word8 -> ST s (V.IPair (MutablePrimArray s Word8))
    go !i !j !k !marr
        | i >= end = return (V.IPair j marr)
        | otherwise = do
            let (# c, d #) = decodeChar arr i
            j' <- encodeChar marr j (f k c)
            let !i' = i + d
                !k' = k + 1
            siz <- sizeofMutableArr marr
            if  j' < siz - 3
            then go i' j' k' marr
            else do
                let !siz' = siz `shiftL` 1
                !marr' <- resizeMutablePrimArray marr siz'
                go i' j' k' marr'

--------------------------------------------------------------------------------
--
-- Strict folds
--

-- | Strict left to right fold.
foldl' :: (b -> Char -> b) -> b -> Text -> b
{-# INLINE foldl' #-}
foldl' f z (Text (V.PrimVector arr s l)) = go z s
  where
    !end = s + l
    -- tail recursive; traverses array left to right
    go !acc !i | i < end  = case decodeChar arr i of
                                (# x, d #) -> go (f acc x) (i + d)
               | otherwise = acc

-- | Strict left to right fold with index.
ifoldl' :: (b -> Int ->  Char -> b) -> b -> Text -> b
{-# INLINE ifoldl' #-}
ifoldl' f z (Text (V.PrimVector arr s l)) = go z s 0
  where
    !end = s + l
    go !acc !i !k | i < end  = case decodeChar arr i of
                                    (# x, d #) -> go (f acc k x) (i + d) (k + 1)
                  | otherwise = acc

-- | Strict right to left fold
foldr' :: (Char -> b -> b) -> b -> Text -> b
{-# INLINE foldr' #-}
foldr' f z (Text (V.PrimVector arr s l)) = go z (s+l-1)
  where
    -- tail recursive; traverses array right to left
    go !acc !i | i >= s    = case decodeCharReverse arr i of
                                (# x, d #) -> go (f x acc) (i - d)
               | otherwise = acc

-- | Strict right to left fold with index
--
-- NOTE: the index is counting from 0, not backwards
ifoldr' :: (Int -> Char -> b -> b) -> b -> Text -> b
{-# INLINE ifoldr' #-}
ifoldr' f z (Text (V.PrimVector arr s l)) = go z (s+l-1) 0
  where
    go !acc !i !k | i >= s    = case decodeCharReverse arr i of
                                    (# x, d #) -> go (f k x acc) (i - d) (k + 1)
                  | otherwise = acc

--------------------------------------------------------------------------------
--
-- Case conversion
--

toCaseFold :: Text -> Text
toCaseFold = toCaseFold' defaultLocale

toCaseFold' :: Locale -> Text -> Text
toCaseFold' locale (Text (V.PrimVector (PrimArray arr#) (I# s#) l@(I# l#)))
    | l == 0 = empty
    | otherwise = unsafeDupablePerformIO $ do
        let l'@(I# l'#) = utf8_casefold_length arr# s# l# locale
        pa@(MutablePrimArray marr#) <- newArr l'
        utf8_casefold arr# s# l# marr# l'# locale
        arr' <- unsafeFreezeArr pa
        let !v = V.fromArr arr' 0 l'
        return (Text v)

foreign import ccall unsafe utf8_casefold ::
    ByteArray# -> Int# -> Int# -> MutableByteArray# RealWorld -> Int# -> Locale -> IO ()
foreign import ccall unsafe utf8_casefold_length ::
    ByteArray# -> Int# -> Int# -> Locale -> Int

concat :: [Text] -> Text
concat = Text . V.concat . coerce
{-# INLINE concat #-}

concatMap :: (Char -> Text) -> Text -> Text
{-# INLINE concatMap #-}
concatMap f = concat . foldr' ((:) . f) []

count :: Char -> Text -> Int
{-# INLINE count #-}
count c (Text v)
    | encodeCharLength c == 1 = let w = V.c2w c in V.count w v
    | otherwise = let (Text pat) = singleton c
                  in List.length $ V.indices pat v False

-- | /O(n)/ Applied to a predicate and a text, 'any' determines
-- if any chars of the text satisfy the predicate.
any :: (Char -> Bool) -> Text -> Bool
{-# INLINE any #-}
any f (Text (V.PrimVector arr s l))
    | l <= 0    = False
    | otherwise = case decodeChar arr s of
                    (# x0, d #) -> go (f x0) (s+d)
  where
    !end = s+l
    go !acc !i | acc       = True
               | i >= end  = acc
               | otherwise = case decodeChar arr i of
                                (# x, d #) -> go (acc || f x) (i+d)

-- | /O(n)/ Applied to a predicate and text, 'all' determines
-- if all chars of the text satisfy the predicate.
all :: (Char -> Bool) -> Text -> Bool
{-# INLINE all #-}
all f (Text (V.PrimVector arr s l))
    | l <= 0    = True
    | otherwise = case decodeChar arr s of
                    (# x0, d #) -> go (f x0) (s+d)
  where
    !end = s+l
    go !acc !i | not acc   = False
               | i >= end  = acc
               | otherwise = case decodeChar arr i of
                                (# x, d #) -> go (acc && f x) (i+d)

--------------------------------------------------------------------------------
--
-- Building text

replicate :: Int -> Char -> Text
replicate 0 _ = empty
replicate n c = Text (V.create siz (go 0))
  where
    !csiz = encodeCharLength c
    !siz = n * csiz
    go :: Int -> MutablePrimArray s Word8 -> ST s ()
    go 0 marr = encodeChar marr 0 c >> go csiz marr
    go i marr | i >= siz = return ()
              | otherwise = do copyChar' csiz marr i marr (i-csiz)
                               go (i+csiz) marr


cycleN :: Int -> Text -> Text
cycleN 0 _ = empty
cycleN n (Text v) = Text (V.cycleN n v)

--------------------------------------------------------------------------------
-- Convert between codepoint vector and text

fromVector :: V.PrimVector Char -> Text
{-# INLINE fromVector #-}
fromVector (V.PrimVector arr s l) = Text (V.createN l (go s 0))
  where
    end = s+l
    go !i !j !marr
        | i >= l = return j
        | otherwise = do
            let c = indexPrimArray arr i
            j' <- encodeChar marr j c
            go (i+1) j' marr

toVector :: Text -> V.PrimVector Char
{-# INLINE toVector #-}
toVector (Text (V.PrimVector arr s l)) = V.createN (l*4) (go s 0)
  where
    end = s+l
    go !i !j !marr
        | i >= l = return j
        | otherwise = do
            let (# c, n #) = decodeChar arr i
            writePrimArray marr j c
            go (i+n) (j+1) marr

