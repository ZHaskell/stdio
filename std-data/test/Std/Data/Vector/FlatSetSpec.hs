{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Vector.FlatSetSpec where

import qualified Data.List                as List
import           Data.Word
import qualified Std.Data.Vector          as V
import qualified Std.Data.Vector.FlatSet  as FS
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

type FMS = FS.FlatSet String

spec :: Spec
spec = do
    describe "flatset-semigroup-monoid" $ do
        prop "flatset monoid unit law" $ \ (m :: FMS)  ->
            (m <> FS.empty) === m
        prop "flatset monoid unit law" $ \ (m :: FMS) ->
            (FS.empty <> m) === m
        prop "flatset semigroup associativity low" $ \ (m1 :: FMS) m2 m3 ->
            (m1 <> m2) <> m3 === m1 <> (m2 <> m3)

    describe "flatset insert elem roundtrip" $ do
        prop "flatset insert elem roundtrip" $ \ (m :: FMS) v ->
            FS.elem v (FS.insert v m) === True

    describe "flatset delete elem" $ do
        prop "flatset delete elem" $ \ (m :: FMS) v ->
            FS.elem v (FS.delete v m) === False
