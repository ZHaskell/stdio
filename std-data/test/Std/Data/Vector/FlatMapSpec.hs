{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Vector.FlatMapSpec where

import qualified Data.List                as List
import           Data.Word
import qualified Std.Data.Vector          as V
import qualified Std.Data.Vector.FlatMap  as FM
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

type FMS = FM.FlatMap String String

spec :: Spec
spec = do
    describe "flatmap-semigroup-monoid" $ do
        prop "flatmap monoid unit law" $ \ (m :: FMS)  ->
            (m <> FM.empty) === m
        prop "flatmap monoid unit law" $ \ (m :: FMS) ->
            (FM.empty <> m) === m
        prop "flatmap semigroup associativity low" $ \ (m1 :: FMS) m2 m3 ->
            (m1 <> m2) <> m3 === m1 <> (m2 <> m3)

    describe "flatmap insert lookup roundtrip" $ do
        prop "flatmap insert lookup roundtrip" $ \ (m :: FMS) k v ->
            FM.lookup k (FM.insert k v m) === Just v

    describe "flatmap delete lookup" $ do
        prop "flatmap delete lookup" $ \ (m :: FMS) k ->
            FM.lookup k (FM.delete k m) === Nothing

    describe "flatmap adjust lookup roundtrip" $ do
        prop "flatmap adjust lookup roundtrip" $ \ (m :: FMS) k (Fun _ f) ->
            FM.lookup k (FM.adjust' f k m) === f `fmap` FM.lookup k m
