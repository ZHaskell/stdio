{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Text.ExtraSpec where

import qualified Data.List                as List
import           Data.Word
import qualified Std.Data.Text.Base        as T
import qualified Std.Data.Text.Extra       as T
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "text extra" $ do

    describe "text reverse . pack == packR" . modifyMaxSuccess (*50) . modifyMaxSize (*50) $ do
        prop "reverse . pack === packR XX" $ \ (UnicodeString xs) ->
            (T.reverse $ T.pack xs) === (T.packR xs)
        prop "reverse . pack === packR XX" $ \ (UnicodeString xs) ->
            (T.reverse $ T.pack xs) === (T.packR xs)
        prop "reverse . pack === packR XX" $ \ (UnicodeString xs) ->
            (T.reverse $ T.pack xs) === (T.packR xs)
