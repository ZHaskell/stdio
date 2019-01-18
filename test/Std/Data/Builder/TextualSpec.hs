{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Builder.TextualSpec where

import qualified Data.List                as List
import           Data.Word
import           Data.Int
import qualified Std.Data.Builder.Textual as T
import qualified Std.Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "textual-builder" $ do
    describe "int roundtrip" $ do
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.int @Word i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.int @Int i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.int @Word64 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.int @Int64 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.int @Word32 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.int @Int32 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.int @Word16 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.int @Int16 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.int @Word8 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.int @Int8 i)

    describe "int roundtrip" $ do
        let f = T.defaultIFormat{T.width = 100, T.padding = T.ZeroPadding}
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.intWith @Word f i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.intWith @Int f i)
        prop "padding length" $ \ i ->
            100 === (T.length . T.buildText $ T.intWith @Word f i)
        prop "padding length" $ \ i ->
            100 === (T.length . T.buildText $ T.intWith @Int f i)

        let f = T.defaultIFormat{T.width = 10, T.padding = T.ZeroPadding}
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.intWith @Word f i)
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.intWith @Int f i)

        let f = T.defaultIFormat{T.width = 10, T.padding = T.LeftSpacePadding}
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.intWith @Word f i)
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.intWith @Int f i)

        let f = T.defaultIFormat{T.width = 10, T.padding = T.RightSpacePadding}
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.intWith @Word f i)
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.intWith @Int f i)

    describe "integer roundtrip" $ do
        prop "integer roundtrip" $ \ i ->
            i === (read . T.unpack . T.buildText $ T.integer i)

        let read' s = read $ "0x" ++ s

        prop "integer roundtrip" $ \ (Positive i) ->
            i === (read' (T.unpack . T.buildText $ T.integerWith T.HexadecimalLower i))

        prop "integer roundtrip" $ \ (Positive i) ->
            i === (read' (T.unpack . T.buildText $ T.integerWith T.HexadecimalUpper i))

    describe "hex roundtrip" $ do

        let read' s = read $ "0x" ++ s

        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.hex @Word i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.hex @Int i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.hex @Word64 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.hex @Int64 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.hex @Word32 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.hex @Int32 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.hex @Word16 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.hex @Int16 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.hex @Word8 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.hex @Int8 i)

    describe "heX roundtrip" $ do

        let read' s = read $ "0x" ++ s

        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.heX @Word i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.heX @Int i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.heX @Word64 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.heX @Int64 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.heX @Word32 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.heX @Int32 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.heX @Word16 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.heX @Int16 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.heX @Word8 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.buildText $ T.heX @Int8 i)
