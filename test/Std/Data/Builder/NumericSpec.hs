{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Builder.NumericSpec where

import qualified Data.List                as List
import           Data.Word
import           Data.Int
import           GHC.Float
import           Text.Printf                 (printf)
import qualified Std.Data.Builder.Numeric as B
import qualified Std.Data.Builder.Base    as B
import qualified Std.Data.Text as T
import qualified Std.Data.Vector as V
import qualified Data.Scientific as Sci
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "builder numeric" . modifyMaxSuccess (*50) . modifyMaxSize (*50) $ do
    describe "int roundtrip" $ do
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.int @Word i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.int @Int i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.int @Word64 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.int @Int64 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.int @Word32 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.int @Int32 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.int @Word16 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.int @Int16 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.int @Word8 i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.int @Int8 i)

    describe "int roundtrip" $ do
        let f = B.defaultIFormat{B.width = 100, B.padding = B.ZeroPadding}
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.intWith @Word f i)
        prop "int roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.intWith @Int f i)
        prop "padding length" $ \ i ->
            100 === (V.length . B.buildBytes $ B.intWith @Word f i)
        prop "padding length" $ \ i ->
            100 === (V.length . B.buildBytes $ B.intWith @Int f i)

        let f = B.defaultIFormat{B.width = 10, B.padding = B.ZeroPadding}
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.intWith @Word f i)
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.intWith @Int f i)

        let f = B.defaultIFormat{B.width = 10, B.padding = B.LeftSpacePadding}
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.intWith @Word f i)
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.intWith @Int f i)

        let f = B.defaultIFormat{B.width = 10, B.padding = B.RightSpacePadding}
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.intWith @Word f i)
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.intWith @Int f i)

    describe "integer roundtrip" $ do
        prop "integer roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.integer i)

    describe "scientific roundtrip" $ do
        prop "scientific roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientific $ Sci.scientific c e)
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientificWith B.FFExponent Nothing $
                    Sci.scientific c e)
        {- FFExponent doesn't roundtrip, i.e. B.scientificWith B.FFExponent (Just 0) 101 ===> 1e2
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientificWith B.FFExponent (Just (abs e)) $
                    Sci.scientific c e)
        -}
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientificWith B.FFGeneric Nothing $
                    Sci.scientific c e)
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientificWith B.FFGeneric (Just (abs e)) $
                    Sci.scientific c e)
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientificWith B.FFFixed Nothing $
                    Sci.scientific c e)
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientificWith B.FFFixed (Just (abs e)) $
                    Sci.scientific c e)

    describe "hex roundtrip" $ do

        let read' s = read $ "0x" ++ s

        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.hex @Word i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.hex @Int i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.hex @Word64 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.hex @Int64 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.hex @Word32 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.hex @Int32 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.hex @Word16 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.hex @Int16 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.hex @Word8 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.hex @Int8 i)

    describe "heX roundtrip" $ do

        let read' s = read $ "0x" ++ s

        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.heX @Word i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.heX @Int i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.heX @Word64 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.heX @Int64 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.heX @Word32 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.heX @Int32 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.heX @Word16 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.heX @Int16 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.heX @Word8 i)
        prop "heX roundtrip" $ \ i ->
            i === (read' . T.unpack . T.validate . B.buildBytes $ B.heX @Int8 i)

    describe "int === show" $ do
        prop "int === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes $ B.int @Word i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes $ B.int @Int i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes $ B.int @Word64 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes $ B.int @Int64 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes $ B.int @Word32 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes $ B.int @Int32 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes $ B.int @Word16 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes $ B.int @Int16 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes $ B.int @Word8 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes $ B.int @Int8 i)

    describe "intWith === printf" $ do
        prop "int === printf" $ \ i ->
            printf "%d" i ===
                (T.unpack . T.validate . B.buildBytes $ B.intWith @Int B.defaultIFormat i)
        prop "int === printf" $ \ i (Positive w) ->
            printf ("%" ++ show w ++ "d") i ===
                (T.unpack . T.validate . B.buildBytes $ B.intWith @Int B.defaultIFormat
                    {B.padding = B.LeftSpacePadding, B.width = w} i)
        prop "int === printf" $ \ i (Positive w) ->
            printf ("%0" ++ show w ++ "d") i ===
                (T.unpack . T.validate . B.buildBytes $ B.intWith @Int B.defaultIFormat
                    {B.padding = B.ZeroPadding, B.width = w} i)
        prop "int === printf" $ \ i (Positive w) ->
            printf ("%-" ++ show w ++ "d") i ===
                (T.unpack . T.validate . B.buildBytes $ B.intWith @Int B.defaultIFormat
                    {B.padding = B.RightSpacePadding, B.width = w} i)

        prop "int === printf" $ \ i ->
            printf "%08x" i ===
                (T.unpack . T.validate . B.buildBytes $ B.hex @Int32 i)

    describe "float, double === show" $ do
        prop "float === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes  $ B.float i)
        prop "double === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes  $ B.double i)

    describe "floatWith, doubleWith === formatRealFloat" $ do
        prop "floatWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFGeneric l i ===
                (T.unpack . T.validate . B.buildBytes  $ B.floatWith FFGeneric l i)
        prop "doubleWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFGeneric l i ===
                (T.unpack . T.validate . B.buildBytes  $ B.doubleWith  FFGeneric l i)
        prop "floatWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFFixed  l  i ===
                (T.unpack . T.validate . B.buildBytes  $ B.floatWith FFFixed  l i)
        prop "doubleWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFFixed  l  i ===
                (T.unpack . T.validate . B.buildBytes  $ B.doubleWith  FFFixed  l i)
        prop "floatWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFExponent l i ===
                (T.unpack . T.validate . B.buildBytes  $ B.floatWith FFExponent l i)
        prop "doubleWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFExponent l i ===
                (T.unpack . T.validate . B.buildBytes  $ B.doubleWith  FFExponent l i)
