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
spec = describe "text-extra" $ do

    describe "T.cons" $ do
        prop "T.cons == List.(:)" $ \ xs x ->
            (T.cons x $ T.pack xs)  === (T.pack . (:) x $ xs)

    describe "T.snoc" $ do
        prop "T.snoc == List.++" $ \ xs x ->
            ((`T.snoc` x) $ T.pack xs)  === (T.pack . (++ [x]) $ xs)

    describe "Text.headMaybe" $ do
        prop "T.headMaybe === Just . list.head" $ \ (NonEmpty xs) ->
            (T.headMaybe $ T.pack xs)  === (Just . List.head $ xs)

    describe "T.initMayEmpty" $ do
        prop "T.initMayEmpty === List.init" $ \ (NonEmpty xs) ->
            (T.initMayEmpty $ T.pack xs)  === (T.pack . List.init $ xs)

    describe "T.lastMaybe" $ do
        prop "T.lastMaybe === Just . list.last" $ \ (NonEmpty xs) ->
            (T.lastMaybe $ T.pack xs)  === (Just . List.last $ xs)

    describe "T.tailMayEmpty" $ do
        prop "T.tailMayEmpty === List.tail" $ \ (NonEmpty xs) ->
            (T.tailMayEmpty $ T.pack xs)  === (T.pack . List.tail $ xs)

    describe "T.take" $ do
        prop "T.take == List.take" $ \ xs x ->
            (T.take x $ T.pack xs)  === (T.pack . List.take x $ xs)

    describe "T.takeR" $ do
        prop "T.takeR x == List.reverse . List.take x . List.reverse" $ \ xs x ->
            (T.takeR x $ T.pack xs)  ===
                (T.pack . List.reverse . List.take x . List.reverse $ xs)

    describe "T.drop" $ do
        prop "T.drop == List.drop" $ \ xs x ->
            (T.drop x $ T.pack xs)  === (T.pack . List.drop x $ xs)

    describe "T.dropR" $ do
        prop "T.dropR x == List.reverse . List.drop x . List.reverse" $ \ xs x ->
            (T.dropR x $ T.pack xs)  ===
                (T.pack . List.reverse . List.drop x . List.reverse $ xs)

    describe "T.slice x y" $ do
        prop "T.slice x y === drop x . take (x+y)" $ \ x y xs ->
            (T.slice x y $ T.pack xs)  === (T.pack . drop x . take (x+y) $ xs)

    describe "T.splitAt" $ do
        prop "T.splitAt == List.splitAt" $ \ xs x ->
            (T.splitAt x $ T.pack xs)  ===
                (let (a,b) = List.splitAt x $ xs in (T.pack a, T.pack b))

    describe "T.takeWhile" $ do
        prop "T.takeWhile == List.takeWhile" $ \ xs (Fun _ x) ->
            (T.takeWhile x $ T.pack xs)  ===
                (T.pack . List.takeWhile x $ xs)

    describe "T.takeWhileR" $ do
        prop "T.takeWhileR == reverse . List.takeWhile . reverse" $ \ xs (Fun _ x) ->
            (T.takeWhileR x $ T.pack xs)  ===
                (T.pack . List.reverse . List.takeWhile x $ List.reverse xs)

    describe "T.dropWhile" $ do
        prop "T.dropWhile == List.dropWhile" $ \ xs (Fun _ x) ->
            (T.dropWhile x $ T.pack xs)  === (T.pack . List.dropWhile x $ xs)

    describe "T.dropWhileR" $ do
        prop "T.dropWhileR == reverse . List.dropWhile . reverse" $ \ xs (Fun _ x) ->
            (T.dropWhileR x $ T.pack xs)  ===
                (T.pack . List.reverse . List.dropWhile x $ List.reverse xs)

    describe "T.break" $ do
        prop "T.break == List.break" $ \ xs (Fun _ x) ->
            (T.break x $ T.pack xs)  ===
                (let (a,b) = List.break x xs in (T.pack a, T.pack b))

    describe "T.breakOn" $ do
        prop "T.breakOn rules" $ \ xs ys ->
            (let (a, b) = T.breakOn (T.pack xs) $ T.pack ys
             in (a `T.append` b, T.pack xs `T.isPrefixOf` b || T.null b) === (T.pack ys, True))

    describe "T.span" $ do
        prop "T.span == List.span" $ \ xs (Fun _ x) ->
            (T.span x $ T.pack xs)  ===
                (let (a,b) = List.span x $ xs in (T.pack a, T.pack b))

    describe "T.breakR" $ do
        prop "T.breakR == List.break in reverse driection" $ \ xs (Fun _ x) ->
            (T.breakR x $ T.pack xs)  ===
                (let (b,a) = List.break x . List.reverse $ xs
                 in (T.reverse $ T.pack a, T.reverse $ T.pack b))

    describe "T.spanR" $ do
        prop "T.spanR == List.span in reverse driection" $ \ xs (Fun _ x) ->
            (T.spanR x $ T.pack xs)  ===
                (let (b,a) = List.span x . List.reverse $ xs
                 in (T.reverse $ T.pack a, T.reverse $ T.pack b))

    describe "T.group" $ do
        prop "T.group == List.group" $ \ xs ->
            (T.group $ T.pack xs)  === (T.pack <$> List.group xs)

    describe "T.groupBy" $ do
        prop "T.groupBy == List.groupBy" $ \ xs x ->
            (T.groupBy (applyFun2 x) $ T.pack xs)  ===
                (T.pack <$> List.groupBy (applyFun2 x) xs)

    describe "T.stripPrefix" $ do
        prop "T.stripPrefix a (a+b) = b " $ \ xs ys ->
            (T.stripPrefix (T.pack xs) . T.pack $ xs++ys) ===
                (Just $ T.pack ys)

    describe "T.stripSuffix" $ do
        prop "T.stripSuffix b (a+b) = a " $ \ xs ys ->
            (T.stripSuffix (T.pack xs) . T.pack $ ys++xs) ===
                (Just $ T.pack ys)

    describe "T.isInfixOf" $ do
        prop "T.isInfixOf b (a+b+c) = True" $ \ xs ys zs ->
            (T.isInfixOf (T.pack xs) . T.pack $ ys++xs++zs) === True

    describe "T.commonPrefix" $ do
        prop "let (c,a,b) = T.commonPrefix x y in (a,b) = (stripPrefix c x,stripPrefix c y)" $ \ xs ys ->
            let (c,a,b) = T.commonPrefix (T.pack xs) $ T.pack ys
                Just xs' = T.stripPrefix c $ T.pack xs
                Just ys' = T.stripPrefix c $ T.pack ys
            in (a,b) === (xs', ys')

    describe "T.intercalate" $ do
        prop "T.intercalate [x] . split x == id" $ \ xs x ->
            (T.intercalate (T.singleton x) . T.split x $ T.pack xs) === T.pack xs

    describe "T.intercalate" $ do
        prop "T.intercalate x . splitOn x == id" $ \ xs x ->
            (T.intercalate (T.pack x) . T.splitOn (T.pack x) $ T.pack xs) ===
                T.pack xs

    describe "T.words" $ do
        prop "T.words === List.words" $ \ xs ->
            (T.words $ T.pack xs)  === (T.pack <$> List.words xs)

    describe "T.lines" $ do
        prop "T.lines === List.lines" $ \ xs ->
            (T.lines $ T.pack xs)  === (T.pack <$> List.lines xs)

    describe "T.unwords" $ do
        prop "T.unwords === List.unwords" $ \ xs ->
            (T.unwords $ List.map T.pack xs)  === (T.pack $ List.unwords xs)

    describe "T.unlines" $ do
        prop "T.unlines === List.unlines" $ \ xs ->
            (T.unlines $ List.map T.pack xs)  === (T.pack $ List.unlines xs)

    describe "T.padLeft" $ do
        prop "T.padLeft n x xs = if l >= n then xs else replicate (n-l) x ++ xs" $ \ xs n x ->
            (T.padLeft n x $ T.pack xs) ===
                (let l = List.length xs
                 in if l >= n then T.pack xs
                              else T.pack $ (List.replicate (n-l) x ++ xs))

    describe "T.padRight" $ do
        prop "T.padRight n x xs = if l >= n then xs else xs ++ List.replicate (n-l) x" $ \ xs n x ->
            (T.padRight n x $ T.pack xs) ===
                (let l = List.length xs
                 in if l >= n then T.pack xs
                              else T.pack $ xs ++ (List.replicate (n-l) x))

    describe "T.reverse" $ do
        prop "reverse . pack === packR XX" $ \ xs ->
            (T.reverse $ T.pack xs) === (T.packR xs)

        prop "unpack reverse === List.reverse" $ \ xs ->
            (T.unpack . T.reverse $ T.pack xs) === (List.reverse xs)

    describe "T.intersperse" $ do
        prop "T.intersperse === List.intersperse" $ \ xs x ->
            (T.intersperse x $ T.pack xs)  ===
                (T.pack . List.intersperse x $ xs)

    describe "T.intercalate" $ do
        prop "T.intercalate === List.intercalate" $ \ xs ys ->
            (T.intercalate (T.pack ys) $ List.map T.pack xs)  ===
                (T.pack . List.intercalate ys $ xs)

    describe "T.intercalateElem" $ do
        prop "T.intercalateElem x === List.intercalate [x]" $ \ xs x ->
            (T.intercalateElem x $ List.map T.pack xs)  ===
                (T.pack . List.intercalate [x] $ xs)

    describe "T.transpose" $ do
        prop "T.transpose === List.transpose" $ \ xss ->
            (T.transpose $ List.map T.pack xss)  === (List.map T.pack $ List.transpose xss)

