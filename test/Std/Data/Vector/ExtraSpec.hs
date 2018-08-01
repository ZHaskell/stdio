{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Vector.ExtraSpec where

import qualified Data.List                as List
import           Data.Word
import qualified Std.Data.Vector          as V
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "vector cons == List.(:)" $ do
        prop "vector cons == List.(:)" $ \ (NonEmpty xs) x ->
            (V.cons x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . (:) x $ xs)
        prop "vector cons == List.(:)" $ \ (NonEmpty xs) x ->
            (V.cons x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . (:) x $ xs)
        prop "vector cons == List.(:)" $ \ (NonEmpty xs) x ->
            (V.cons x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . (:) x $ xs)

    describe "vector snoc == List.++" $ do
        prop "vector snoc == List.++" $ \ (NonEmpty xs) x ->
            ((`V.snoc` x) . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . (++ [x]) $ xs)
        prop "vector snoc == List.++" $ \ (NonEmpty xs) x ->
            ((`V.snoc` x) . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . (++ [x]) $ xs)
        prop "vector snoc == List.++" $ \ (NonEmpty xs) x ->
            ((`V.snoc` x) . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . (++ [x]) $ xs)

    describe "vector headMaybe == Just. List.head" $ do
        prop "vector headMaybe === Just . list.head" $ \ (NonEmpty xs) ->
            (V.headMaybe . V.pack @V.Vector @Integer $ xs)  ===
                (Just . List.head $ xs)
        prop "vector headMaybe === Just . List.head" $ \ (NonEmpty xs) ->
            (V.headMaybe . V.pack @V.PrimVector @Int $ xs)  ===
                (Just . List.head $ xs)
        prop "vector headMaybe === Just . List.head" $ \ (NonEmpty xs) ->
            (V.headMaybe . V.pack @V.PrimVector @Word8 $ xs)  ===
                (Just . List.head $ xs)

    describe "vector initMayEmpty == List.init" $ do
        prop "vector initMayEmpty === List.init" $ \ (NonEmpty xs) ->
            (V.initMayEmpty . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.init $ xs)
        prop "vector initMayEmpty === List.init" $ \ (NonEmpty xs) ->
            (V.initMayEmpty . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.init $ xs)
        prop "vector initMayEmpty === List.init" $ \ (NonEmpty xs) ->
            (V.initMayEmpty . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.init $ xs)

    describe "vector lastMaybe == Just. List.last" $ do
        prop "vector lastMaybe === Just . list.last" $ \ (NonEmpty xs) ->
            (V.lastMaybe . V.pack @V.Vector @Integer $ xs)  ===
                (Just . List.last $ xs)
        prop "vector lastMaybe === Just . List.last" $ \ (NonEmpty xs) ->
            (V.lastMaybe . V.pack @V.PrimVector @Int $ xs)  ===
                (Just . List.last $ xs)
        prop "vector lastMaybe === Just . List.last" $ \ (NonEmpty xs) ->
            (V.lastMaybe . V.pack @V.PrimVector @Word8 $ xs)  ===
                (Just . List.last $ xs)

    describe "vector tailMayEmpty == List.tail" $ do
        prop "vector tailMayEmpty === List.tail" $ \ (NonEmpty xs) ->
            (V.tailMayEmpty . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.tail $ xs)
        prop "vector tailMayEmpty === List.tail" $ \ (NonEmpty xs) ->
            (V.tailMayEmpty . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.tail $ xs)
        prop "vector tailMayEmpty === List.tail" $ \ (NonEmpty xs) ->
            (V.tailMayEmpty . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.tail $ xs)

    describe "vector take == List.take" $ do
        prop "vector take == List.take" $ \ (NonEmpty xs) x ->
            (V.take x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.take x $ xs)
        prop "vector take == List.take" $ \ (NonEmpty xs) x ->
            (V.take x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.take x $ xs)
        prop "vector take == List.take" $ \ (NonEmpty xs) x ->
            (V.take x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.take x $ xs)

    describe "vector drop == List.drop" $ do
        prop "vector drop == List.drop" $ \ (NonEmpty xs) x ->
            (V.drop x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.drop x $ xs)
        prop "vector drop == List.drop" $ \ (NonEmpty xs) x ->
            (V.drop x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.drop x $ xs)
        prop "vector drop == List.drop" $ \ (NonEmpty xs) x ->
            (V.drop x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.drop x $ xs)

    describe "vector slice x y == drop x . take (x+y)" $ do
        prop "vector slice x y === drop x . take (x+y)" $ \ x y xs ->
            (V.slice x y  . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . drop x . take (x+y) $ xs)
        prop "vector slice x y xs === drop x . take (x+y) x" $ \ x y xs ->
            (V.slice x y  . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . drop x . take (x+y) $ xs)
        prop "vector slice x y xs === drop x . take (x+y) x" $ \ x y xs ->
            (V.slice x y  . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . drop x . take (x+y) $ xs)

    describe "vector (|..|) rules, see the document for (|..|)" $ do
        let f x y vs = let l = V.length vs
                           x' = if x >= 0 then x else l+x
                           y' = if y >= 0 then y else l+y
                       in V.slice x' (y'-x'+1) vs
        prop "vector (|..|) rules" $ \ x y xs ->
            (V.pack @V.Vector @Integer xs V.|..| (x,y))  === (f x y . V.pack $ xs)
        prop "vector (|..|) rules" $ \ x y xs ->
            (V.pack @V.PrimVector @Int xs V.|..| (x,y))  === (f x y . V.pack $ xs)
        prop "vector (|..|) rules" $ \ x y xs ->
            (V.pack @V.PrimVector @Word8 xs V.|..| (x,y))  === (f x y . V.pack $ xs)

    describe "vector intercalate == List.intercalate" $ do
        prop "vector intercalate === List.intercalate" $ \ xs ys ->
            (V.intercalate (V.pack ys) . List.map (V.pack @V.Vector @Integer) $ xs)  ===
                (V.pack . List.intercalate ys $ xs)
        prop "vector intercalate ys === List.intercalate x" $ \ xs ys ->
            (V.intercalate (V.pack ys) . List.map (V.pack @V.PrimVector @Int) $ xs)  ===
                (V.pack . List.intercalate ys $ xs)
        prop "vector intercalate ys === List.intercalate x" $ \ xs ys ->
            (V.intercalate (V.pack ys) . List.map (V.pack @V.PrimVector @Word8) $ xs)  ===
                (V.pack . List.intercalate ys $ xs)

    describe "vector reverse == List.reverse" $ do
        prop "vector reverse === List.reverse" $ \ xs ->
            (V.reverse . V.pack @V.Vector @Integer $ xs)  === (V.pack . List.reverse $ xs)
        prop "vector reverse === List.reverse" $ \ xs ->
            (V.reverse . V.pack @V.PrimVector @Int $ xs)  === (V.pack . List.reverse $ xs)
        prop "vector reverse === List.reverse" $ \ xs ->
            (V.reverse . V.pack @V.PrimVector @Word8 $ xs)  === (V.pack . List.reverse $ xs)

    describe "vector intersperse == List.intersperse" $ do
        prop "vector intersperse === List.intersperse" $ \ xs x ->
            (V.intersperse x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.intersperse x $ xs)
        prop "vector intersperse x === List.intersperse x" $ \ xs x ->
            (V.intersperse x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.intersperse x $ xs)
        prop "vector intersperse x === List.intersperse x" $ \ xs x ->
            (V.intersperse x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.intersperse x $ xs)

    describe "vector scanl' == List.scanl" $ do
        prop "vector scanl' === List.scanl" $ \ xs f x ->
            (V.scanl' @V.Vector @V.Vector (applyFun2 f :: Integer -> Integer -> Integer) x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.scanl (applyFun2 f) x $ xs)
        prop "vector scanl' x === List.scanl x" $ \ xs f x ->
            (V.scanl' @V.PrimVector @V.PrimVector (applyFun2 f :: Int -> Int -> Int) x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.scanl (applyFun2 f) x $ xs)
        prop "vector scanl' x === List.scanl x" $ \ xs f x ->
            (V.scanl' @V.PrimVector @V.Vector (applyFun2 f :: Int -> Word8 -> Int) x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.scanl (applyFun2 f) x $ xs)

    describe "vector scanr' == List.scanr" $ do
        prop "vector scanr' === List.scanr" $ \ xs f x ->
            (V.scanr' @V.Vector @V.Vector (applyFun2 f :: Integer -> Integer -> Integer) x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.scanr (applyFun2 f) x $ xs)
        prop "vector scanr' x === List.scanr x" $ \ xs f x ->
            (V.scanr' @V.PrimVector @V.PrimVector (applyFun2 f :: Int -> Int -> Int) x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.scanr (applyFun2 f) x $ xs)
        prop "vector scanr' x === List.scanr x" $ \ xs f x ->
            (V.scanr' @V.PrimVector @V.Vector (applyFun2 f :: Word8 -> Int -> Int) x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.scanr (applyFun2 f) x $ xs)
