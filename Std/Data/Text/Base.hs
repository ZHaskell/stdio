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

A 'Text' simply wrap a 'Bytes' which will be interpreted using UTF-8 encoding, illegal UTF-8 encoded codepoints will be interpreted as the @\U+FFFD@ replacement character. You can use 'validateUTF8' to check if a 'Text' value
contain illegal byte sequence.

-}

module Std.Data.Text.Base (
  -- * Text type
    Text(..)
  , charAt
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
import           Std.Data.Vector.Base     (Bytes, PrimVector)
import qualified Std.Data.Vector.Base     as V
import qualified Std.Data.Vector.Extra    as V
import           Std.Data.Parser   (Result(..))

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
charAt (Text (V.PrimVector ba s l)) n | n < 0 = Nothing
                              | otherwise = go s 0
  where
    !end = s + l
    go !i !j
        | i >= end = Nothing
        | j >= n = let !c = decodeChar_ ba i in Just c
        | otherwise =
            let l = decodeCharLen ba i in go (i+l) (j+1)

--------------------------------------------------------------------------------

data ValidateResult
    = ValidateSuccess !Text
    | ValidatePartialBytes !Text  !Bytes
    | ValidateInvalidBytes !Bytes !Bytes
  deriving (Show, Eq)

-- | /O(n)/ Validate a sequence of bytes is UTF-8 encoded.
--
-- This function only copy as little as possible: i.e. only characters spanning
-- across boundray will be merged into a single character 'Text'.
validateUTF8 :: Bytes -> Result Text
{-# INLINE validateUTF8 #-}
validateUTF8 bs@(V.PrimVector ba s l) | l == 0    = Success bs empty
                                      | otherwise = go s
  where
    end = s + l
    go !i = case validateChar ba i end of
                r
                    | r > 0  -> go (i + r)
                    | r == 0 ->
                        if i > s
                        then Success
                                (V.PrimVector ba i (end-i))
                                (Text (V.PrimVector ba s (i-s)))
                        else NeedMore (more bs)
                    | otherwise ->
                        Failure
                            (V.PrimVector ba i (end-i))
                            [ "Illegal bytes: " ++ show (V.PrimVector ba i (-r)) ]
    more trailing input
        | V.null input = Failure trailing  [ "Trailing bytes: " ++ show trailing ]
        | otherwise =
            let trailing'@(V.PrimVector ba s l) = trailing' `V.append` V.take 3 input
            in case validateChar ba s l of
                r
                    | r > 0  ->
                        Success
                            (V.drop (r - V.length trailing) input)
                            (Text (V.take r trailing'))
                    | r == 0 ->
                        NeedMore (more trailing')
                    | otherwise ->
                        Failure
                            (V.drop (r - V.length trailing) input)
                            [ "Illegal bytes: " ++ show (V.take (-r) trailing') ]

-- | /O(n)/ Repair UTF-8 bytes by convert illegal codepoint to replacement char @\U+FFFD@
-- (encoded as @0xEF 0xBF 0xBD@ 3 bytes).
--
-- This function always perform allocation and copying. It never return 'Failure', so be
-- careful using this function, since it may silently convert non UTF-8 encoded bytes into
-- garbage.
--
-- The replacing process follows Option #2 in
-- <http://www.unicode.org/review/pr-121.html Public Review Issue #121>.
--
-- Partial trailing bytes are converted into a single replacement char.
repairUTF8 :: Bytes -> Result Text
repairUTF8 bs@(V.PrimVector ba s l) | l == 0    = Success bs empty
                                    | otherwise = runST $ do
    mba <- newPrimArray l
    go s 0 mba
  where
    end = s + l
    go !i !j !mba =
        case validateChar ba i end of
            0 ->
                if i > s
                then do
                    ba' <- unsafeFreezePrimArray mba
                    return (Success
                        (V.PrimVector ba i (end-i))
                        (Text (V.PrimVector ba' 0 j)))
                else return (NeedMore (more bs))

            1 -> do writePrimArray mba j $ indexPrimArray ba i
                    go (i+1) (j+1)
            2 -> do writePrimArray mba j $ indexPrimArray ba i
                    writePrimArray mba (j+1) $ indexPrimArray ba (i+1)
                    go (i+2) (j+2)
            3 -> do writePrimArray mba j $ indexPrimArray ba i
                    writePrimArray mba (j+1) $ indexPrimArray ba (i+1)
                    writePrimArray mba (j+2) $ indexPrimArray ba (i+2)
                    go (i+3) (j+3)
            4 -> do writePrimArray mba j $ indexPrimArray ba i
                    writePrimArray mba (j+1) $ indexPrimArray ba (i+1)
                    writePrimArray mba (j+2) $ indexPrimArray ba (i+2)
                    writePrimArray mba (j+3) $ indexPrimArray ba (i+3)
                    go (i+4) (j+4)
            r -> do writePrimArray mba j 0xEF
                    writePrimArray mba (j+1) 0xBF
                    writePrimArray mba (j+2) 0xBD
                    go (i-r) (j+3)

    more trailing input
        | V.null input = Failure trailing  [ "Trailing bytes: " ++ show trailing ]
        | otherwise = do
            mba <- newPrimArray l
            go s 0 mba
            let trailing'@(V.PrimVector ba s l) = trailing' `V.append` V.take 3 input
            in case validateChar ba s l of
                r
                    | r > 0  ->
                        Success
                            (V.drop (r - V.length trailing) input)
                            (Text (V.take r trailing'))
                    | r == 0 ->
                        NeedMore (more trailing')
                    | otherwise ->
                        Failure
                            (V.drop (r - V.length trailing) input)
                            [ "Illegal bytes: " ++ show (V.take (-r) trailing') ]


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

