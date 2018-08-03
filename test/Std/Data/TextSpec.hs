{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.TextSpec where

import qualified Data.List                as List
import           Data.Word
import qualified Std.Data.Text            as T
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "text" $ do
    describe "UTF8Bytes instance property" $ do
        prop "text eq === string eq" $ \ xs ys ->
            (T.pack xs == T.pack ys) === (xs == ys)

        prop "text compare === string compare" $ \ xs ys ->
            (T.pack xs `compare` T.pack ys) === (xs `compare` ys)

    describe "UTF8Bytes unpack . pack == id" $ do
        prop "unpack . pack === id" $ \ xs ->
            T.unpack (T.pack xs)  === xs


