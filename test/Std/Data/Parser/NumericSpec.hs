{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Parser.NumericSpec where

import qualified Data.List                as L
import           Data.Word
import           Data.Int
import           GHC.Float
import           Text.Printf                 (printf)
import           Data.Word8                  (toLower, toUpper)
import qualified Std.Data.Parser.Numeric    as P
import qualified Std.Data.Parser.Base    as P
import qualified Std.Data.Builder.Numeric    as B
import qualified Std.Data.Builder.Base    as B
import qualified Std.Data.Text as T
import qualified Std.Data.Vector.Base as V
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "numeric parsers roundtrip" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "positive hex roundtrip" $ \ i ->
            P.parse P.hex (B.buildBytes (B.hex i)) == Right (i :: Int)
        prop "positive hex roundtrip" $ \ i ->
            P.parse P.hex (B.buildBytes (B.hex i)) == Right (i :: Int64)
        prop "positive hex roundtrip" $ \ i ->
            P.parse P.hex (B.buildBytes (B.hex i)) == Right (i :: Int32)
        prop "positive hex roundtrip" $ \ i ->
            P.parse P.hex (B.buildBytes (B.hex i)) == Right (i :: Int16)
        prop "positive hex roundtrip" $ \ i ->
            P.parse P.hex (B.buildBytes (B.hex i)) == Right (i :: Int8)
        prop "positive hex roundtrip" $ \ i ->
            P.parse P.hex (B.buildBytes (B.hex i)) == Right (i :: Word)
        prop "positive hex roundtrip" $ \ i ->
            P.parse P.hex (B.buildBytes (B.hex i)) == Right (i :: Word64)
        prop "positive hex roundtrip" $ \ i ->
            P.parse P.hex (B.buildBytes (B.hex i)) == Right (i :: Word32)
        prop "positive hex roundtrip" $ \ i ->
            P.parse P.hex (B.buildBytes (B.hex i)) == Right (i :: Word16)
        prop "positive hex roundtrip" $ \ i ->
            P.parse P.hex (B.buildBytes (B.hex i)) == Right (i :: Word8)


        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse P.uint (B.buildBytes (B.int i)) == Right (i :: Int)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse P.uint (B.buildBytes (B.int i)) == Right (i :: Int64)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse P.uint (B.buildBytes (B.int i)) == Right (i :: Int32)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse P.uint (B.buildBytes (B.int i)) == Right (i :: Int16)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse P.uint (B.buildBytes (B.int i)) == Right (i :: Int8)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse P.uint (B.buildBytes (B.int i)) == Right (i :: Word)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse P.uint (B.buildBytes (B.int i)) == Right (i :: Word64)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse P.uint (B.buildBytes (B.int i)) == Right (i :: Word32)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse P.uint (B.buildBytes (B.int i)) == Right (i :: Word16)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse P.uint (B.buildBytes (B.int i)) == Right (i :: Word8)


        prop "positive int roundtrip" $ \ i ->
            P.parse P.int (B.buildBytes (B.int i)) == Right (i :: Int)
        prop "positive int roundtrip" $ \ i ->
            P.parse P.int (B.buildBytes (B.int i)) == Right (i :: Int64)
        prop "positive int roundtrip" $ \ i ->
            P.parse P.int (B.buildBytes (B.int i)) == Right (i :: Int32)
        prop "positive int roundtrip" $ \ i ->
            P.parse P.int (B.buildBytes (B.int i)) == Right (i :: Int16)
        prop "positive int roundtrip" $ \ i ->
            P.parse P.int (B.buildBytes (B.int i)) == Right (i :: Int8)
        prop "positive int roundtrip" $ \ i ->
            P.parse P.int (B.buildBytes (B.int i)) == Right (i :: Word)
        prop "positive int roundtrip" $ \ i ->
            P.parse P.int (B.buildBytes (B.int i)) == Right (i :: Word64)
        prop "positive int roundtrip" $ \ i ->
            P.parse P.int (B.buildBytes (B.int i)) == Right (i :: Word32)
        prop "positive int roundtrip" $ \ i ->
            P.parse P.int (B.buildBytes (B.int i)) == Right (i :: Word16)
        prop "positive int roundtrip" $ \ i ->
            P.parse P.int (B.buildBytes (B.int i)) == Right (i :: Word8)


        prop "float roundtrip" $ \ i ->
            P.parse P.float (B.buildBytes (B.float i)) == Right (i :: Float)
        prop "double roundtrip" $ \ i ->
            P.parse P.double (B.buildBytes (B.double i)) == Right (i :: Double)
