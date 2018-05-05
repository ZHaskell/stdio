{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Property.Vector where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Function
import Test.QuickCheck
import qualified Data.Vector as V
import Data.Word
import qualified Data.List as List

propertyVector :: TestTree
propertyVector = testGroup "vector property" [
        testProperty "vector eq === list eq" . property $ \ xs ys ->
            (V.pack @V.Vector @Int xs == V.pack ys) === (xs == ys)
    ,   testProperty "vector eq === list eq" . property $ \ xs ys ->
            (V.pack @V.PrimVector @Int xs == V.pack ys) === (xs == ys)
    ,   testProperty "vector eq === list eq" . property $ \ xs ys ->
            (V.pack @V.PrimVector @Word8 xs == V.pack ys) === (xs == ys)

    ,   testProperty "vector compare === list compare" . property $ \ xs ys ->
            (V.pack @V.Vector @Int xs `compare` V.pack ys) === (xs `compare` ys)
    ,   testProperty "vector compare === list compare" . property $ \ xs ys ->
            (V.pack @V.PrimVector @Int xs `compare` V.pack ys) === (xs `compare` ys)
    ,   testProperty "vector compare === list compare" . property $ \ xs ys ->
            (V.pack @V.PrimVector @Word8 xs `compare` V.pack ys) === (xs `compare` ys)

    ,   testProperty "unpack . pack === id" . property $ \ xs ->
            (V.unpack @V.Vector @Int) (V.pack xs)  === xs
    ,   testProperty "unpack . pack === id" . property $ \ xs ->
            (V.unpack @V.PrimVector @Int) (V.pack xs)  === xs
    ,   testProperty "unpack . pack === id" . property $ \ xs ->
            (V.unpack @V.PrimVector @Word8) (V.pack xs)  === xs

    ,   testProperty "unpackR . packR === id" . property $ \ xs ->
            (V.unpackR @V.Vector @Int) (V.packR xs)  === xs
    ,   testProperty "unpackR . packR === id" . property $ \ xs ->
            (V.unpackR @V.PrimVector @Int) (V.packR xs)  === xs
    ,   testProperty "unpackR . packR === id" . property $ \ xs ->
            (V.unpackR @V.PrimVector @Word8) (V.packR xs)  === xs

    ,   testProperty "pack === packN XX" . property $ \ xs d ->
            (V.pack @V.Vector @Int xs) === (V.packN d xs)
    ,   testProperty "pack === packN XX" . property $ \ xs d ->
            (V.pack @V.PrimVector @Int xs) === (V.packN d xs)
    ,   testProperty "pack === packN XX" . property $ \ xs d ->
            (V.pack @V.PrimVector @Word8 xs) === (V.packN d xs)

    ,   testProperty "packR === packRN XX" . property $ \ xs d ->
            (V.packR @V.Vector @Int xs) === (V.packRN d xs)
    ,   testProperty "packR === packRN XX" . property $ \ xs d ->
            (V.packR @V.PrimVector @Int xs) === (V.packRN d xs)
    ,   testProperty "packR === packRN XX" . property $ \ xs d ->
            (V.packR @V.PrimVector @Word8 xs) === (V.packRN d xs)

    ,   testProperty "reverse . pack === packR XX" . property $ \ xs ->
            (V.reverse $ V.pack @V.Vector @Int xs) === (V.packR xs)
    ,   testProperty "reverse . pack === packR XX" . property $ \ xs ->
            (V.reverse $ V.pack @V.PrimVector @Int xs) === (V.packR xs)
    ,   testProperty "reverse . pack === packR XX" . property $ \ xs ->
            (V.reverse $ V.pack @V.PrimVector @Word8 xs) === (V.packR xs)

    ,   testProperty "vector length === list length" . property $ \ xs ->
            (V.length $ V.pack @V.Vector @Int xs)  ===  List.length xs
    ,   testProperty "vector length === list length" . property $ \ xs ->
            (V.length $ V.pack @V.PrimVector @Int xs)  ===  List.length xs
    ,   testProperty "vector length === list length" . property $ \ xs ->
            (V.length $ V.pack @V.PrimVector @Word8 xs)  ===  List.length xs

    ,   testProperty "vector init === list init" . property $ \ (NonEmpty xs) ->
            (V.init . V.pack @V.Vector @Int $ xs)  ===
                (V.pack . List.init $ xs)
    ,   testProperty "vector init === list init" . property $ \ (NonEmpty xs) ->
            (V.init . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.init $ xs)
    ,   testProperty "vector init === list init" . property $ \ (NonEmpty xs) ->
            (V.init . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.init $ xs)

    ,   testProperty "vector last === list last" . property $ \ (NonEmpty xs) ->
            (V.last . V.pack @V.Vector @Int $ xs)  ===
                (List.last $ xs)
    ,   testProperty "vector last === list last" . property $ \ (NonEmpty xs) ->
            (V.last . V.pack @V.PrimVector @Int $ xs)  ===
                (List.last $ xs)
    ,   testProperty "vector last === list last" . property $ \ (NonEmpty xs) ->
            (V.last . V.pack @V.PrimVector @Word8 $ xs)  ===
                (List.last $ xs)

    ,   testProperty "vector map === list map" . property $ \ xs (Fun _ f) ->
            (V.map @V.Vector @V.Vector (f :: Int -> Integer) $ V.pack @V.Vector @Int xs) ===
                (V.pack $ List.map f xs)
    ,   testProperty "vector map === list map" . property $ \ xs (Fun _ f)  ->
            (V.map @V.PrimVector @V.PrimVector (f :: Int -> Word8) $ V.pack @V.PrimVector @Int xs) ===
                (V.pack $ List.map f xs)
    ,   testProperty "vector map === list map" . property $ \ xs (Fun _ f) ->
            (V.map @V.PrimVector @V.PrimVector (f :: Word8 -> Int) $ V.pack @V.PrimVector @Word8 xs) ===
                (V.pack $ List.map f xs)
    ,   testProperty "vector map === list map" . property $ \ xs (Fun _ f) ->
            (V.map @V.PrimVector @V.Vector (f :: Word8 -> Integer) $ V.pack @V.PrimVector @Word8 xs) ===
                (V.pack $ List.map f xs)
    ,   testProperty "vector map === list map" . property $ \ xs (Fun _ f) ->
            (V.map @V.Vector @V.PrimVector (f :: Integer -> Word8) $ V.pack @V.Vector @Integer xs) ===
                (V.pack $ List.map f xs)

    ,   testProperty "vector reverse === list reverse" . property $ \ xs ->
            (V.reverse . V.pack @V.Vector @Int $ xs)  === (V.pack . List.reverse $ xs)
    ,   testProperty "vector reverse === list reverse" . property $ \ xs ->
            (V.reverse . V.pack @V.PrimVector @Int $ xs)  === (V.pack . List.reverse $ xs)
    ,   testProperty "vector reverse === list reverse" . property $ \ xs ->
            (V.reverse . V.pack @V.PrimVector @Word8 $ xs)  === (V.pack . List.reverse $ xs)

    ,   testProperty "vector intersperse === list intersperse" . property $ \ xs x ->
            (V.intersperse x . V.pack @V.Vector @Int $ xs)  ===
                (V.pack . List.intersperse x $ xs)
    ,   testProperty "vector intersperse x === list intersperse x" . property $ \ xs x ->
            (V.intersperse x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.intersperse x $ xs)
    ,   testProperty "vector intersperse x === list intersperse x" . property $ \ xs x ->
            (V.intersperse x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.intersperse x $ xs)

    ,   testProperty "vector intercalate === list intercalate" . property $ \ xs ys ->
            (V.intercalate (V.pack ys) . List.map (V.pack @V.Vector @Int) $ xs)  ===
                (V.pack . List.intercalate ys $ xs)
    ,   testProperty "vector intercalate ys === list intercalate x" . property $ \ xs ys ->
            (V.intercalate (V.pack ys) . List.map (V.pack @V.PrimVector @Int) $ xs)  ===
                (V.pack . List.intercalate ys $ xs)
    ,   testProperty "vector intercalate ys === list intercalate x" . property $ \ xs ys ->
            (V.intercalate (V.pack ys) . List.map (V.pack @V.PrimVector @Word8) $ xs)  ===
                (V.pack . List.intercalate ys $ xs)

    ,   testProperty "vector foldl' === list foldl'" . property $ \ xs f x ->
            (V.foldl' (applyFun2 f :: Int -> Int -> Int) x . V.pack @V.Vector @Int $ xs)  ===
                (List.foldl' (applyFun2 f) x $ xs)
    ,   testProperty "vector foldl' x === list foldl' x" . property $ \ xs f x ->
            (V.foldl' (applyFun2 f :: Int -> Int -> Int) x . V.pack @V.PrimVector @Int $ xs)  ===
                (List.foldl' (applyFun2 f) x $ xs)
    ,   testProperty "vector foldl' x === list foldl' x" . property $ \ xs f x ->
            (V.foldl' (applyFun2 f :: Int -> Word8 -> Int) x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (List.foldl' (applyFun2 f) x $ xs)

    ,   testProperty "vector foldr' === list foldr" . property $ \ xs f x ->
            (V.foldr' (applyFun2 f :: Int -> Int -> Int) x . V.pack @V.Vector @Int $ xs)  ===
                (List.foldr (applyFun2 f) x $ xs)
    ,   testProperty "vector foldr' x === list foldr' x" . property $ \ xs f x ->
            (V.foldr' (applyFun2 f :: Int -> Int -> Int) x . V.pack @V.PrimVector @Int $ xs)  ===
                (List.foldr (applyFun2 f) x $ xs)
    ,   testProperty "vector foldr' x === list foldr' x" . property $ \ xs f x ->
            (V.foldr' (applyFun2 f :: Word8 -> Int -> Int) x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (List.foldr (applyFun2 f) x $ xs)

    ,   testProperty "vector concat === list concat" . property $ \ xss ->
            (V.concat . List.map (V.pack @V.Vector @Int) $ xss)  === (V.pack . List.concat $ xss)
    ,   testProperty "vector concat === list concat" . property $ \ xss ->
            (V.concat . List.map (V.pack @V.PrimVector @Int) $ xss) === (V.pack . List.concat $ xss)
    ,   testProperty "vector concat === list concat" . property $ \ xss ->
            (V.concat . List.map (V.pack @V.PrimVector @Word8) $ xss) === (V.pack . List.concat $ xss)

    ,   testProperty "vector concatMap === list concatMap" . property $ \ xss (Fun _ f) ->
            (V.concatMap (V.pack @V.Vector @Int . f) . V.pack $ xss)  === (V.pack . List.concatMap f $ xss)
    ,   testProperty "vector concatMap === list concatMap" . property $ \ xss (Fun _ f) ->
            (V.concatMap (V.pack @V.PrimVector @Int . f) . V.pack $ xss) === (V.pack . List.concatMap f $ xss)
    ,   testProperty "vector concatMap === list concatMap" . property $ \ xss (Fun _ f) ->
            (V.concatMap (V.pack @V.PrimVector @Word8 . f) . V.pack $ xss) === (V.pack . List.concatMap f $ xss)

    ,   testProperty "vector all === list all" . property $ \ xs (Fun _ f) ->
            (V.all f . V.pack @V.Vector @Int $ xs)  === (List.all f $ xs)
    ,   testProperty "vector all === list all" . property $ \ xs (Fun _ f) ->
            (V.all f . V.pack @V.PrimVector @Int $ xs)  === (List.all f $ xs)
    ,   testProperty "vector all === list all" . property $ \ xs (Fun _ f) ->
            (V.all f . V.pack @V.PrimVector @Word8 $ xs)  === (List.all f $ xs)

    ,   testProperty "vector any === list any" . property $ \ xs (Fun _ f) ->
            (V.any f . V.pack @V.Vector @Int $ xs)  === (List.any f $ xs)
    ,   testProperty "vector any === list any" . property $ \ xs (Fun _ f) ->
            (V.any f . V.pack @V.PrimVector @Int $ xs)  === (List.any f $ xs)
    ,   testProperty "vector any === list any" . property $ \ xs (Fun _ f) ->
            (V.any f . V.pack @V.PrimVector @Word8 $ xs)  === (List.any f $ xs)

    ,   testProperty "vector maximum === list maximum" . property $ \ (NonEmpty xs) ->
            (V.maximum . V.pack @V.Vector @Int $ xs)  === (List.maximum $ xs)
    ,   testProperty "vector maximum === list maximum" . property $ \ (NonEmpty xs) ->
            (V.maximum . V.pack @V.PrimVector @Int $ xs)  === (List.maximum $ xs)
    ,   testProperty "vector maximum === list maximum" . property $ \ (NonEmpty xs) ->
            (V.maximum . V.pack @V.PrimVector @Word8 $ xs)  === (List.maximum $ xs)

    ,   testProperty "vector scanl === list scanl" . property $ \ xs f x ->
            (V.scanl @V.Vector @V.Vector (applyFun2 f :: Int -> Int -> Int) x . V.pack @V.Vector @Int $ xs)  ===
                (V.pack . List.scanl (applyFun2 f) x $ xs)
    ,   testProperty "vector scanl x === list scanl x" . property $ \ xs f x ->
            (V.scanl @V.PrimVector @V.PrimVector (applyFun2 f :: Int -> Int -> Int) x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.scanl (applyFun2 f) x $ xs)
    ,   testProperty "vector scanl x === list scanl x" . property $ \ xs f x ->
            (V.scanl @V.PrimVector @V.Vector (applyFun2 f :: Int -> Word8 -> Int) x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.scanl (applyFun2 f) x $ xs)

    ,   testProperty "vector scanr === list scanr" . property $ \ xs f x ->
            (V.scanr @V.Vector @V.Vector (applyFun2 f :: Int -> Int -> Int) x . V.pack @V.Vector @Int $ xs)  ===
                (V.pack . List.scanr (applyFun2 f) x $ xs)
    ,   testProperty "vector scanr x === list scanr x" . property $ \ xs f x ->
            (V.scanr @V.PrimVector @V.PrimVector (applyFun2 f :: Int -> Int -> Int) x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.scanr (applyFun2 f) x $ xs)
    ,   testProperty "vector scanr x === list scanr x" . property $ \ xs f x ->
            (V.scanr @V.PrimVector @V.Vector (applyFun2 f :: Word8 -> Int -> Int) x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.scanr (applyFun2 f) x $ xs)
    ]
