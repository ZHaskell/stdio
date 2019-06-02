{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Std.Data.TextBuilderSpec where

import qualified Data.List                as L
import           Data.Word
import           Data.Int
import           GHC.Generics
import qualified Std.Data.Text            as T
import           Std.Data.TextBuilder
import           Std.Data.JSON            (Value)
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck


data T a
    = Nullary
    | Unary Int
    | Product T.Text (Maybe Char) a
    | Record { testOne   :: Double
             , testTwo   :: Maybe Bool
             , testThree :: Maybe a
             }
    | List [a]
   deriving (Show, Eq, ToText, Generic)

data I a = I a :+ I a | I a :- I a | J a deriving (Show, Generic, ToText)
infixr 5 :+
infixl 6 :-

spec :: Spec
spec = describe "JSON Base instances" $ do

    it "Nullary constructor are encoded as text" $
        toText (Nullary :: T Integer) === "Nullary"

    it "Unary constructor are encoded as single field" $
        toText (Unary 123456 :: T Integer) === "Unary 123456"

    it "Product are encoded as multiple field" $
        toText (Product "ABC" (Just 'x') (123456::Integer)) ===
            "Product \"ABC\" (Just 'x') 123456"

    it "Record are encoded as key values" $
        toText (Record 0.123456 Nothing (Just (123456::Integer))) ===
            "Record {testOne = 0.123456, testTwo = Nothing, testThree = Just 123456}"

    it "List are encode as array" $
        toText (List [Nullary
            , Unary 123456
            , (Product "ABC" (Just 'x') (123456::Integer))
            , (Record 0.123456 Nothing (Just (123456::Integer)))]) ===
                "List [Nullary,Unary 123456,Product \"ABC\" (Just 'x') 123456,\
                    \Record {testOne = 0.123456, testTwo = Nothing, testThree = Just 123456}]"

    it "infix constructor should respect piority" $
        toString (J 1 :- J 2 :+ J 3 :- J 4 :- J 5 :+ J 6 :+ J 7 :+ J 8 :- J 9 :- J 10 :- J 11 :: I Int)
            === show (J 1 :- J 2 :+ J 3 :- J 4 :- J 5 :+ J 6 :+ J 7 :+ J 8 :- J 9 :- J 10 :- J 11)

    prop "Value Show instance === ToText instances" $ \ (v :: Value) ->
        toString v === show v
