{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.CBytesSpec where

import qualified Data.List                as List
import           Data.Word
import           Data.Hashable            (hashWithSalt, hash)
import qualified Std.Data.CBytes          as CB
import qualified Std.Data.Vector.Base     as V
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "CBytes-base" $ do
    describe "CBytes Eq Ord property" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "CBytes eq === List.eq" $ \ xs ys ->
            (CB.pack xs == CB.pack ys) === (xs == ys)

        prop "CBytes compare === List.compare" $ \ xs ys ->
            let xs' = List.filter (/= '\NUL') xs
                ys' = List.filter (/= '\NUL') ys
            in (CB.pack xs' `compare` CB.pack ys') === (xs' `compare` ys')

    describe "CBytes Hashable instance property" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "CBytes a's hash should be equal to Bytes's hash" $ \ (ASCIIString xs) ->
            let ys = List.filter (/= '\NUL') xs
            in hash (CB.pack ys) === hash (V.packASCII ys)
        prop "CBytes a's hash should be equal to literal's hash" $
            hash ("hello world!" :: CB.CBytes) === hash (CB.fromBytes "hello world!")

    describe "CBytes IsString instance property" $ do
        prop "ASCII string" $
            "hello world" === CB.fromText "hello world"
        prop "UTF8 string" $
            "你好世界" === CB.fromText "你好世界"

    describe "CBytes length == List.length" $ do
        prop "CBytes length === List.length" $ \ (ASCIIString xs) ->
            let ys = List.filter (/= '\NUL') xs
            in (CB.length $ CB.pack ys)  ===  List.length ys

    describe "CBytes append == List.(++)" $ do
        prop "CBytes eq === List.eq" $ \ xs ys ->
            (CB.pack xs `CB.append` CB.pack ys) === CB.pack (xs ++ ys)
