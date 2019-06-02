{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Text.SearchSpec where

import qualified Data.List                as List
import           Data.Word
import qualified Std.Data.Text.Base        as T
import qualified Std.Data.Text.Extra        as T
import qualified Std.Data.Text.Search      as T
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "text-search" $ do

    describe "T.elem == List.elem" $ do
        prop "T.elem = List.elem" $ \ y x ->
            (T.elem y $ T.pack x) === (List.elem y $ x)

    describe "snd . T.find == List.find" $ do
        prop "snd .T.find = List.find" $ \ (Fun _ y) x ->
            (case T.find y . T.pack $ x of (_, _, c) -> c)  === (List.find y $ x)

    describe "T.find" $ do
        prop "T.find = maybe List.length List.findIndexOrEnd" $ \ (Fun _ y) x ->
            (case T.find y . T.pack $ x of (i,_,_) -> i) ===
                (maybe (List.length x) id $ List.findIndex y x)

    describe "T.findR" $ do
        prop "T.find = findR . reverse" $ \ (Fun _ y) x ->
            (case T.find y . T.pack $ x of (i,_,_) -> i) ===
                (case T.findR y . T.reverse $ T.pack x of (i,_,_) -> i)

    describe "T.filter == List.filter" $ do
        prop "T.filter = List.filter" $ \ (Fun _ y) x ->
            (T.filter y . T.pack $ x) === (T.pack $ List.filter y $ x)

    describe "T.partition == List.partition" $ do
        prop "T.partition = List.partition" $ \ (Fun _ y) x ->
            (T.partition y . T.pack $ x) ===
                (let (a,b) = List.partition y $ x in (T.pack a, T.pack b))

