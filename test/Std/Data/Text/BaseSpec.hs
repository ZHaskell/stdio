{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Text.BaseSpec where

import qualified Data.List                as List
import           Data.Word
import qualified Std.Data.Text.Base       as T
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "text base" $ do
    describe "text Eq Ord property" $ do
        prop "text eq === List.eq" $ \ (UnicodeString xs) (UnicodeString ys) ->
            (T.pack xs == T.pack ys) === (xs == ys)

        prop "text compare === List.compare" $ \ (UnicodeString xs) (UnicodeString ys) ->
            (T.pack xs `compare` T.pack ys) === (xs `compare` ys)

    describe "text unpack(R) . pack(R)(N) == id/reverse" . modifyMaxSuccess (*50) . modifyMaxSize (*50) $ do
        prop "unpack . pack === id" $ \ (UnicodeString xs)  ->
            (T.unpack (T.pack xs))  === xs

        prop "unpackR . pack === reverse" $ \ (UnicodeString xs)  ->
            (T.unpackR (T.pack xs))  ===  reverse xs

        prop "unpack . packR === reverse" $ \ (UnicodeString xs)  ->
            (T.unpack (T.packR xs))  ===  reverse xs

        prop "unpackR . packR === id" $ \ (UnicodeString xs)  ->
            (T.unpackR (T.packR xs))  === xs

    describe "text pack == packN" . modifyMaxSuccess (*50) . modifyMaxSize (*50) $ do
        prop "pack === packN XX" $ \ (UnicodeString xs) d ->
            (T.pack xs) === (T.packN d xs)

        prop "packR === packRN XX" $ \ (UnicodeString xs) d ->
            (T.packR xs) === (T.packRN d xs)


    describe "text length == List.length" $ do
        prop "text length === List.length" $ \ (UnicodeString xs) ->
            (T.length $ T.pack xs)  ===  List.length xs

    describe "text append == List.(++)" $ do
        prop "text eq === List.eq" $ \ (UnicodeString xs) (UnicodeString ys) ->
            (T.unpack $ T.pack xs `T.append` T.pack ys) === (xs ++ ys)

    describe "text map' == List.map" $ do
        prop "text map' === List.map" $ \ (UnicodeString xs) (Fun _ f) ->
            (T.map' f (T.pack xs)) === (T.pack $ List.map f xs)

    describe "text foldl' == List.foldl'" $ do
        prop "text foldl' === List.foldl'" $ \ (UnicodeString xs) f x ->
            (T.foldl' (applyFun2 f :: Char -> Char -> Char) x (T.pack xs))  ===
                (List.foldl' (applyFun2 f) x $ xs)

    describe "text foldr' == List.foldr'" $ do
        prop "text foldr' === List.foldr" $ \ (UnicodeString xs) f x ->
            (T.foldr' (applyFun2 f :: Char -> Char -> Char) x (T.pack xs))  ===
                (List.foldr (applyFun2 f) x $ xs)

    describe "text concat == List.concat" $ do
        prop "text concat === List.concat" $ \ xss ->
            (T.concat $ List.map (T.pack . getUnicodeString) xss)  ===
                (T.pack . List.concat $ List.map getUnicodeString xss)

    describe "text concatMap == List.concatMap" $ do
        prop "text concatMap === List.concatMap" $ \ xs (Fun _ f) ->
            (T.concatMap (T.pack . f) . T.pack . getUnicodeString) xs  ===
                (T.pack . List.concatMap f $ getUnicodeString xs)

    describe "text all == List.all" $ do
        prop "text all === List.all" $ \ (UnicodeString xs) (Fun _ f) ->
            (T.all f $ T.pack xs)  === (List.all f $ xs)

    describe "text any == List.any" $ do
        prop "text any === List.any" $ \ (UnicodeString xs) (Fun _ f) ->
            (T.any f $ T.pack xs)  === (List.any f $ xs)

    describe "text count x == List.length . List.filter (==x)" $ do
        prop "text count === List.length . List.filter (==x)" $ \ (UnicodeString xs) x ->
            (T.count x $ T.pack xs)  === (List.length . List.filter (==x) $ xs)

    describe "text replicate == List.replicate" $ do
        prop "text replicate = List.replicate" $ \ n x ->
            (T.replicate n x) == (T.pack (List.replicate n $ x))

