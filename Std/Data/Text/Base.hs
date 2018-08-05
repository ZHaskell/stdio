{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : Std.Data.Text
Description : Unicode text processing
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

A 'Text' which wrap a 'Bytes' that are correctly UTF-8 encoded codepoints. WIP

-}

module Std.Data.Text.Base (
  -- * Text type
    Text(..)
  , (!!)
  -- * Basic creating
  , empty, singleton
  -- * Conversion between list
  , pack, packR
  , unpack, unpackR
  -- * Basic interface
  , null
  , length
  , append
  , map', imap'
  , foldl', ifoldl', foldl1', foldl1Maybe'
  , foldr', ifoldr', foldr1', foldr1Maybe'
    -- ** Case conversion
  , toCaseFold, toLower, toUpper, toTitle
    -- ** Special folds
  , concat, concatMap
  , maximumMaybe, minimumMaybe
  , count
  , all, any
  -- * Building vector
  -- ** Accumulating maps
  , mapAccumL
  , mapAccumR
  -- ** Generating and unfolding vector
  , replicate
  , repeatN
  , unfoldr
  , unfoldrN
 ) where

import           Control.DeepSeq
import           Control.Monad.ST
import           Data.Bits
import           Data.Char
import           Data.Foldable            (foldlM)
import qualified Data.List                as List
import           Data.Primitive.PrimArray
import           Data.Typeable
import           Data.Word
import           GHC.Exts                 (build)
import           GHC.Prim
import           GHC.Types
import           Std.Data.Array
import           Std.Data.Text.UTF8Codec
import           Std.Data.Vector.Base     (Bytes, pattern Vec)
import qualified Std.Data.Vector.Base     as V
import qualified Std.Data.Vector.Extra    as V

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

--------------------------------------------------------------------------------

data ValidateResult
    = ValidateSuccess !Text
    | ValidatePartialBytes !Text  !Bytes
    | ValidateInvalidBytes !Bytes !Bytes
  deriving (Show, Eq)

-- | /O(n)/ Validate a sequence of bytes is UTF-8 encoded.
validateUTF8 :: Bytes -> ValidateResult
{-# INLINE validateUTF8 #-}
validateUTF8 bs@(V.PrimVector ba s l) = go s
  where
    end = s + l
    go :: Int -> ValidateResult
    go !i
        | i < end = case validateChar ba i end of
            r
                | r > 0  -> go (i + r)
                | r == 0 ->
                    PartialBytes
                        (Text (V.PrimVector ba s (i-s)))
                        (V.PrimVector ba i (end-i))
                | otherwise ->
                    InvalidBytes
                        (V.PrimVector ba i (-r))
        | otherwise = Success (Text bs)

data RepairResult
    = RepairSuccess !Text
    | RepairPartialBytes !Text  !Bytes

-- | /O(n)/ Repair UTF-8 bytes by convert illegal codepoint to replacement char @\U+FFFD@.
--
-- https://stackoverflow.com/questions/2547262/why-is-python-decode-replacing-more-than-the-invalid-bytes-from-an-encoded-strin

repairUTF8 :: Bytes -> RepairResult
repairUTF8 (Vec arr s l) = runST $ do
    ma <- newArr l
    go 0 ma
  where
    end = s + l
    go :: Int -> MutablePrimArray Word8 -> ValidateResult
    go !i
        | i < end = case validateChar ba i end of
            r
                | r > 0  -> go (i + r)
                | r == 0 ->
                    PartialBytes
                        (Text (V.PrimVector ba s (i-s)))
                        (V.PrimVector ba i (end-i))
                | otherwise ->
                    InvalidBytes
                        (V.PrimVector ba i (-r))
        | otherwise = Success (Text bs)

{-
fromUTF8 :: Bytes -> (Text, Bytes)
fromUTF8Lenient :: Bytes -> (Text, Bytes)
toUTF8 :: Text -> Bytes

fromUTF16 :: Bytes -> (Text, Bytes)
fromUTF16Lenient :: Bytes -> (Text, Bytes)
toUTF16 :: Text -> Bytes

-}

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
packN n0 = \ ws0 -> runST (do mba <- newPrimArray n0
                              (SP2 i mba') <- foldlM go (SP2 0 mba) ws0
                              shrinkMutablePrimArray mba' i
                              ba <- unsafeFreezePrimArray mba'
                              return (Text (V.fromArr ba 0 i))
                          )
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: SP2 s -> Char -> ST s (SP2 s)
    go (SP2 i mba) !c = do
        siz <- getSizeofMutablePrimArray mba
        if i < siz - 3  -- we need at least 4 bytes for safety
        then do
            i' <- encodeChar mba i c
            return (SP2 i' mba)
        else do
            let !siz' = siz `shiftL` 1
            !mba' <- resizeMutablePrimArray mba siz'
            i' <- encodeChar mba' i c
            return (SP2 i' mba')

data SP2 s = SP2 {-# UNPACK #-}!Int {-# UNPACK #-}!(MutablePrimArray s Word8)


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
singleton c = Text $ V.createN 4 $ \ mba -> encodeChar mba 0 c

empty :: Text
{-# INLINABLE empty #-}
empty = Text V.empty

--------------------------------------------------------------------------------
-- * Basic interface

cons :: Char -> Text -> Text
{-# INLINABLE cons #-}
cons c (Text (V.PrimVector ba s l)) = Text $ V.createN (4 + l) $ \ mba -> do
        i <- encodeChar mba 0 c
        copyPrimArray mba i ba s l
        return $! i + l

snoc :: Text -> Char -> Text
{-# INLINABLE snoc #-}
snoc (Text (V.PrimVector ba s l)) c = Text $ V.createN (4 + l) $ \ mba -> do
    copyPrimArray mba 0 ba s l
    encodeChar mba l c

append :: Text -> Text -> Text
append ta tb = Text ( getUTF8Bytes ta `V.append` getUTF8Bytes tb )
{-# INLINE append #-}

uncons :: Text -> Maybe (Char, Text)
{-# INLINE uncons #-}
uncons (Text (V.PrimVector ba s l))
    | l == 0  = Nothing
    | otherwise =
        let (# c, i #) = decodeChar ba s
        in Just (c, Text (V.PrimVector ba (s+i) (l-i)))

unsnoc :: Text -> Maybe (Text, Char)
{-# INLINE unsnoc #-}
unsnoc (Text (V.PrimVector ba s l))
    | l == 0  = Nothing
    | otherwise =
        let (# c, i #) = decodeCharReverse ba (s + l - 1)
        in Just (Text (V.PrimVector ba s (l-i)), c)

head :: Text -> Char
{-# INLINABLE head #-}
head t = case uncons t of { Nothing -> errorEmptyText "head"; Just (c, _) -> c }

tail :: Text -> Text
{-# INLINABLE tail #-}
tail t = case uncons t of { Nothing -> empty; Just (_, t) -> t }

last :: Text -> Char
{-# INLINABLE last #-}
last t = case unsnoc t of { Nothing -> errorEmptyText "last"; Just (_, c) -> c }

init :: Text -> Text
{-# INLINABLE init #-}
init t = case unsnoc t of { Nothing -> empty; Just (t, _) -> t }

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
map :: (Char -> Char) -> Text -> Text
map f = \ t@(Text v) -> packN (V.length v + 3) (List.map f (unpack t)) -- the 3 bytes buffer is here for optimizing ascii mapping
{-# INLINE map #-}                                                     -- because we do resize if less than 3 bytes left when building

-- | /O(n)/ The 'intercalate' function takes a 'Text' and a list of
-- 'Text's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: Text -> [Text] -> Text
intercalate t = concat . (List.intersperse t)
{-# INLINE intercalate #-}

concat :: [Text] -> Text
concat = Text . V.concat . (List.map getUTF8Bytes) -- (coerce :: [Text] -> [Bytes])
{-# INLINE concat #-}


--------------------------------------------------------------------------------

errorEmptyText :: String -> a
{-# NOINLINE errorEmptyText #-}
errorEmptyText fun = error ("Data.Text." ++ fun ++ ": empty Text")

--------------------------------------------------------------------------------

