{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Std.Data.JSON.BaseSpec where

import qualified Data.List                as L
import           Data.Word
import           Data.Int
import           GHC.Generics
import qualified Std.Data.Text            as T
import qualified Std.Data.Builder         as B
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck
import qualified Std.Data.JSON as JSON
import           Std.Data.JSON (FromValue, ToValue, EncodeJSON)


data T a
    = Nullary
    | Unary Int
    | Product T.Text (Maybe Char) a
    | Record { testOne   :: Double
             , testTwo   :: Maybe Bool
             , testThree :: Maybe a
             }
    | List [a]
   deriving (Show, Eq, Generic, FromValue, ToValue, EncodeJSON)

spec :: Spec
spec = describe "JSON Base instances" $ do

    it "Nullary constructor are encoded as text" $
        JSON.encodeText (Nullary :: T Integer) === "\"Nullary\""

    it "Unary constructor are encoded as single field object" $
        JSON.encodeText (Unary 123456 :: T Integer) === "{\"Unary\":123456}"

    it "Product are encoded as array" $
        JSON.encodeText (Product "ABC" (Just 'x') (123456::Integer)) ===
            "{\"Product\":[\"ABC\",\"x\",123456]}"

    it "Record are encoded as key values" $
        JSON.encodeText (Record 0.123456 Nothing (Just (123456::Integer))) ===
            "{\"Record\":{\
                \\"testOne\":0.123456,\
                \\"testTwo\":null,\
                \\"testThree\":123456}}"

    it "List are encode as array" $
        JSON.encodeText (List [Nullary
            , Unary 123456
            , (Product "ABC" (Just 'x') (123456::Integer))
            , (Record 0.123456 Nothing (Just (123456::Integer)))]) ===
                "{\"List\":[\"Nullary\",\
                \{\"Unary\":123456},\
                \{\"Product\":[\"ABC\",\"x\",123456]},\
                \{\"Record\":{\
                \\"testOne\":0.123456,\
                \\"testTwo\":null,\
                \\"testThree\":123456}}]}"

    it "control characters are escaped" $
        JSON.encodeText (T.pack $ map toEnum [0..0x1F]) ===
            "\"\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007\\b\\t\\n\\u000b\\f\\r\\u000e\\u000f\
            \\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017\\u0018\\u0019\\u001a\\u001b\\u001c\\u001d\\u001e\\u001f\""
