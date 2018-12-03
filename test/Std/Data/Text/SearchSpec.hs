{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Text.SearchSpec where

import qualified Data.List                as List
import           Data.Word
import qualified Std.Data.Text.Base        as T
import qualified Std.Data.Text.Search      as T
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "text search" $ do

    describe "text elem == List.elem" $ do
        prop "text elem = List.elem" $ \ y x ->
            (T.elem y $ T.pack x) === (List.elem y $ x)

