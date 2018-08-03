{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Vector.SortSpec where

import qualified Data.List                as List
import           Data.Word
import           Data.Int
import qualified Std.Data.Vector          as V
import qualified Std.Data.Vector.Sort     as V
import           Std.Data.Vector.Sort     (Radix(..))
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

data StableTest = StableTest { key :: Int, payload :: Integer } deriving (Eq, Show)
instance Ord StableTest where
    StableTest x1 y1 `compare` StableTest x2 y2 = x1 `compare` x2

instance V.Radix StableTest where
    bucketSize = bucketSize . key
    passes = passes . key
    radixLSB = radixLSB . key
    radix i = radix i . key
    radixMSB = radixMSB . key

spec :: Spec
spec = describe "vector sort" $ do
    describe "vector insertSort == List.sort" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "vector insertSort == List.sort" $ \ xs ->
            (V.insertSort . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector insertSort == List.sort" $ \ xs ->
            (V.insertSort . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector insertSort == List.sort" $ \ xs ->
            (V.insertSort . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack $ List.sort xs)

    describe "vector insertSort should be stable" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "vector insertSort should be stable" $ \ xs ys ->
            (V.insertSort . V.pack @V.Vector $ List.zipWith StableTest xs ys)  ===
                (V.pack $ List.sort $ List.zipWith StableTest xs ys)

    describe "vector mergeSort == List.sort" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "vector mergeSort == List.sort" $ \ xs ->
            (V.mergeSort . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector mergeSort == List.sort" $ \ xs ->
            (V.mergeSort . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector mergeSort == List.sort" $ \ xs ->
            (V.mergeSort . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack $ List.sort xs)

    describe "vector mergeSort should be stable" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "vector mergeSort should be stable" $ \ xs ys ->
            (V.mergeSort . V.pack @V.Vector $ List.zipWith StableTest xs ys)  ===
                (V.pack $ List.sort $ List.zipWith StableTest xs ys)

    describe "vector radixSort == List.sort" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "vector radixSort == List.sort" $ \ xs ->
            (V.radixSort . V.pack @V.Vector @Int $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector radixSort == List.sort" $ \ xs ->
            (V.radixSort . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector radixSort == List.sort" $ \ xs ->
            (V.radixSort . V.pack @V.PrimVector @Int64 $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector radixSort == List.sort" $ \ xs ->
            (V.radixSort . V.pack @V.PrimVector @Int32 $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector radixSort == List.sort" $ \ xs ->
            (V.radixSort . V.pack @V.PrimVector @Int16 $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector radixSort == List.sort" $ \ xs ->
            (V.radixSort . V.pack @V.PrimVector @Int8 $ xs)  ===
                (V.pack $ List.sort xs)

        prop "vector radixSort == List.sort" $ \ xs ->
            (V.radixSort . V.pack @V.PrimVector @Word $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector radixSort == List.sort" $ \ xs ->
            (V.radixSort . V.pack @V.PrimVector @Word64 $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector radixSort == List.sort" $ \ xs ->
            (V.radixSort . V.pack @V.PrimVector @Word32 $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector radixSort == List.sort" $ \ xs ->
            (V.radixSort . V.pack @V.PrimVector @Word16 $ xs)  ===
                (V.pack $ List.sort xs)
        prop "vector radixSort == List.sort" $ \ xs ->
            (V.radixSort . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack $ List.sort xs)

    describe "vector radixSort should be reversed by RadixDown" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "vector radixSort should be reversed by RadixDown" $ \ xs ->
            (V.castVector . V.radixSort . V.pack @V.Vector @Int $ xs)  ===
                (V.reverse . V.radixSort . V.pack $ map V.RadixDown xs)
        prop "vector radixSort should be reversed by RadixDown" $ \ xs ->
            (V.castVector . V.radixSort . V.pack @V.PrimVector @Int $ xs)  ===
                (V.reverse . V.radixSort . V.pack $ map V.RadixDown xs)
        prop "vector radixSort should be reversed by RadixDown" $ \ xs ->
            (V.castVector . V.radixSort . V.pack @V.PrimVector @Int64 $ xs)  ===
                (V.reverse . V.radixSort . V.pack $ map V.RadixDown xs)
        prop "vector radixSort should be reversed by RadixDown" $ \ xs ->
            (V.castVector . V.radixSort . V.pack @V.PrimVector @Int32 $ xs)  ===
                (V.reverse . V.radixSort . V.pack $ map V.RadixDown xs)
        prop "vector radixSort should be reversed by RadixDown" $ \ xs ->
            (V.castVector . V.radixSort . V.pack @V.PrimVector @Int16 $ xs)  ===
                (V.reverse . V.radixSort . V.pack $ map V.RadixDown xs)
        prop "vector radixSort should be reversed by RadixDown" $ \ xs ->
            (V.castVector . V.radixSort . V.pack @V.PrimVector @Int8 $ xs)  ===
                (V.reverse . V.radixSort . V.pack $ map V.RadixDown xs)

        prop "vector radixSort should be reversed by RadixDown" $ \ xs ->
            (V.castVector . V.radixSort . V.pack @V.PrimVector @Word $ xs)  ===
                (V.reverse . V.radixSort . V.pack $ map V.RadixDown xs)
        prop "vector radixSort should be reversed by RadixDown" $ \ xs ->
            (V.castVector . V.radixSort . V.pack @V.PrimVector @Word64 $ xs)  ===
                (V.reverse . V.radixSort . V.pack $ map V.RadixDown xs)
        prop "vector radixSort should be reversed by RadixDown" $ \ xs ->
            (V.castVector . V.radixSort . V.pack @V.PrimVector @Word32 $ xs)  ===
                (V.reverse . V.radixSort . V.pack $ map V.RadixDown xs)
        prop "vector radixSort should be reversed by RadixDown" $ \ xs ->
            (V.castVector . V.radixSort . V.pack @V.PrimVector @Word16 $ xs)  ===
                (V.reverse . V.radixSort . V.pack $ map V.RadixDown xs)
        prop "vector radixSort should be reversed by RadixDown" $ \ xs ->
            (V.castVector . V.radixSort . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.reverse . V.radixSort . V.pack $ map V.RadixDown xs)

    describe "vector radixSort should be stable" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "vector radixSort should be stable" $ \ xs ys ->
            (V.radixSort . V.pack @V.Vector $ List.zipWith StableTest xs ys)  ===
                (V.pack $ List.sort $ List.zipWith StableTest xs ys)
