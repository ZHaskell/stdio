{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.JSON.ValueSpec where

import qualified Data.List                as L
import           Data.Word
import           Data.Int
import           GHC.Float
import           Data.Word8                  (toLower, toUpper)
import qualified Std.Data.Builder         as B
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck
import qualified Std.Data.JSON.Value as JSON
import qualified Std.Data.JSON.Builder as JSONB


spec :: Spec
spec = describe "JSON" $ do -- large size will generate too huge JSON document
    prop "value roundtrip" $ \ v ->
        Right v === JSON.parseValue' (B.buildBytes (JSONB.value v))
