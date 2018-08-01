{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Vector.SearchSpec where

import qualified Data.List                as List
import           Data.Word
import qualified Std.Data.Vector          as V
import qualified Std.Data.Vector.Search   as V
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "vector elemIndices == List.elemIndices" $ do
        prop "vector elemIndices = List.elemIndices" $ \ y x ->
            (V.elemIndices y . V.pack @V.Vector @Integer $ x)  === (List.elemIndices y $ x)
        prop "vector elemIndices = List.elemIndices" $ \ y x ->
            (V.elemIndices y . V.pack @V.PrimVector @Int $ x)  === (List.elemIndices y $ x)
        prop "vector elemIndices = List.elemIndices" $ \ y x ->
            (V.elemIndices y . V.pack @V.PrimVector @Word8 $ x)  === (List.elemIndices y $ x)

    describe "vector elemIndexOrEndBytes == List.elemIndex" $ do
        prop "vector splitAt == List.splitAt" $ \ xs x ->
            (V.elemIndexOrEndBytes x . V.pack $ xs)  ===
                (case List.elemIndex x xs of Just r -> r
                                             Nothing -> List.length xs)

