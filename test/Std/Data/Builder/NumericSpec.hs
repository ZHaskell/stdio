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
import           Test.QuickCheck.Instances.Scientific
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

    describe "c_intWith == hs_intWith" $ do
        prop "c_intWith == hs_intWith @Word" $ \ i f ->
            (B.buildBytes $ B.hs_intWith f i) === (B.buildBytes $ B.c_intWith @Word f i)
        prop "c_intWith == hs_intWith @Word8" $ \ i f ->
            (B.buildBytes $ B.hs_intWith f i) === (B.buildBytes $ B.c_intWith @Word8 f i)
        prop "c_intWith == hs_intWith @Word16" $ \ i f ->
            (B.buildBytes $ B.hs_intWith f i) === (B.buildBytes $ B.c_intWith @Word16 f i)
        prop "c_intWith == hs_intWith @Word32" $ \ i f ->
            (B.buildBytes $ B.hs_intWith f i) === (B.buildBytes $ B.c_intWith @Word32 f i)
        prop "c_intWith == hs_intWith @Word64" $ \ i f ->
            (B.buildBytes $ B.hs_intWith f i) === (B.buildBytes $ B.c_intWith @Word64 f i)
        prop "c_intWith == hs_intWith @Int" $ \ i f ->
            (B.buildBytes $ B.hs_intWith f i) === (B.buildBytes $ B.c_intWith @Int f i)
        prop "c_intWith == hs_intWith @Int8" $ \ i f ->
            (B.buildBytes $ B.hs_intWith f i) === (B.buildBytes $ B.c_intWith @Int8 f i)
        prop "c_intWith == hs_intWith @Int16" $ \ i f ->
            (B.buildBytes $ B.hs_intWith f i) === (B.buildBytes $ B.c_intWith @Int16 f i)
        prop "c_intWith == hs_intWith @Int32" $ \ i f ->
            (B.buildBytes $ B.hs_intWith f i) === (B.buildBytes $ B.c_intWith @Int32 f i)
        prop "c_intWith == hs_intWith @Int64" $ \ i f ->
            (B.buildBytes $ B.hs_intWith f i) === (B.buildBytes $ B.c_intWith @Int64 f i)

    describe "integer roundtrip" $ do
        prop "integer roundtrip" $ \ i ->
            i === (read . T.unpack . T.validate . B.buildBytes $ B.integer i)
        prop "integer roundtrip II" $
            -- there're an issue with leading zeros in front of an block, so we add a case manually here
            (2132132100000000000000000000000000213213 :: Integer) ===
                (read . T.unpack . T.validate . B.buildBytes $ B.integer 2132132100000000000000000000000000213213)

    describe "scientific roundtrip" $ do
        prop "scientific roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientific $ Sci.scientific c e)
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientificWith B.Exponent Nothing $
                    Sci.scientific c e)
        {- B.Exponent doesn't roundtrip, i.e. B.scientificWith B.Exponent (Just 0) 101 ===> 1e2
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientificWith B.Exponent (Just (abs e)) $
                    Sci.scientific c e)
        -}
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientificWith B.Generic Nothing $
                    Sci.scientific c e)
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientificWith B.Generic (Just (abs e)) $
                    Sci.scientific c e)
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientificWith B.Fixed Nothing $
                    Sci.scientific c e)
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . T.validate . B.buildBytes . B.scientificWith B.Fixed (Just (abs e)) $
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
        prop "int === printf %d" $ \ i ->
            printf "%d" i ===
                (T.unpack . T.validate . B.buildBytes $ B.intWith @Int B.defaultIFormat i)
        prop "int === printf %xxd" $ \ i (Positive w) ->
            printf ("%" ++ show w ++ "d") i ===
                (T.unpack . T.validate . B.buildBytes $ B.intWith @Int B.defaultIFormat
                    {B.padding = B.LeftSpacePadding, B.width = w} i)
        prop "int === printf %0xxd" $ \ i (Positive w) ->
            printf ("%0" ++ show w ++ "d") i ===
                (T.unpack . T.validate . B.buildBytes $ B.intWith @Int B.defaultIFormat
                    {B.padding = B.ZeroPadding, B.width = w} i)
        prop "int === printf %-xx%" $ \ i (Positive w) ->
            printf ("%-" ++ show w ++ "d") i ===
                (T.unpack . T.validate . B.buildBytes $ B.intWith @Int B.defaultIFormat
                    {B.padding = B.RightSpacePadding, B.width = w} i)
        prop "hex === printf %08x" $ \ i ->
            printf "%08x" i ===
                (T.unpack . T.validate . B.buildBytes $ B.hex @Int32 i)

    describe "float, double === show" $ do
        prop "float === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes  $ B.float i)
        prop "double === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes  $ B.double i)
        prop "scientific === show" $ \ i ->
            show i === (T.unpack . T.validate . B.buildBytes  $ B.scientific i)

    describe "floatWith, doubleWith === formatRealFloat" $ do
        prop "floatWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFGeneric l i ===
                (T.unpack . T.validate . B.buildBytes  $ B.floatWith B.Generic l i)
        prop "doubleWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFGeneric l i ===
                (T.unpack . T.validate . B.buildBytes  $ B.doubleWith  B.Generic l i)
        prop "floatWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFFixed  l  i ===
                (T.unpack . T.validate . B.buildBytes  $ B.floatWith B.Fixed  l i)
        prop "doubleWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFFixed  l  i ===
                (T.unpack . T.validate . B.buildBytes  $ B.doubleWith  B.Fixed  l i)
        prop "floatWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFExponent l i ===
                (T.unpack . T.validate . B.buildBytes  $ B.floatWith B.Exponent l i)
        prop "doubleWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFExponent l i ===
                (T.unpack . T.validate . B.buildBytes  $ B.doubleWith B.Exponent l i)

    describe "grisu3, grisu3_sp === floatToDigits 10" $ do
        prop "grisu3 === floatToDigits" $ \ (Positive f) ->
            B.grisu3 f === floatToDigits 10 f
        prop "grisu3_sp === floatToDigits" $ \ (Positive f) ->
            B.grisu3_sp f === floatToDigits 10 f
