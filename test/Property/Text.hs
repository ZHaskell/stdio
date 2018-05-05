{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Property.Text where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Function
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Char
import qualified Data.List as List

propertyText :: TestTree
propertyText = testGroup "text property" [
        testProperty "text eq === string eq" . property $ \ xs ys ->
            (T.pack xs == T.pack ys) === (xs == ys)

    ,   testProperty "text compare === string compare" . property $ \ xs ys ->
            (T.pack xs `compare` T.pack ys) === (xs `compare` ys)

    ,   testProperty "unpack . pack === id" . property $ \ xs ->
            (T.unpack) (T.pack xs)  === xs


    ]
