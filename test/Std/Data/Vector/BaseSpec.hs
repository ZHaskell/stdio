{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Vector.BaseSpec where

import qualified Data.List                as List
import           Data.Word
import           Data.Hashable            (hashWithSalt, hash)
import qualified Std.Data.Vector.Base     as V
import qualified Std.Foreign.PrimArray    as FP
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "vector-base" $ do
    describe "vector Eq Ord property" $ do
        prop "vector eq === List.eq" $ \ xs ys ->
            (V.pack @V.Vector @Integer xs == V.pack ys) === (xs == ys)
        prop "vector eq === List.eq" $ \ xs ys ->
            (V.pack @V.PrimVector @Int xs == V.pack ys) === (xs == ys)
        prop "vector eq === List.eq" $ \ xs ys ->
            (V.pack @V.PrimVector @Word8 xs == V.pack ys) === (xs == ys)

        prop "vector compare === List.compare" $ \ xs ys ->
            (V.pack @V.Vector @Integer xs `compare` V.pack ys) === (xs `compare` ys)
        prop "vector compare === List.compare" $ \ xs ys ->
            (V.pack @V.PrimVector @Int xs `compare` V.pack ys) === (xs `compare` ys)
        prop "vector compare === List.compare" $ \ xs ys ->
            (V.pack @V.PrimVector @Word8 xs `compare` V.pack ys) === (xs `compare` ys)

    describe "vector unpack(R) . pack(R)(N) == id" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "unpack . pack === id" $ \ xs ->
            (V.unpack @V.Vector @Integer) (V.pack xs)  === xs
        prop "unpack . pack === id" $ \ xs ->
            (V.unpack @V.PrimVector @Int) (V.pack xs)  === xs
        prop "unpack . pack === id" $ \ xs ->
            (V.unpack @V.PrimVector @Word8) (V.pack xs)  === xs

        prop "unpackR . packR === id" $ \ xs ->
            (V.unpackR @V.Vector @Integer) (V.packR xs)  === xs
        prop "unpackR . packR === id" $ \ xs ->
            (V.unpackR @V.PrimVector @Int) (V.packR xs)  === xs
        prop "unpackR . packR === id" $ \ xs ->
            (V.unpackR @V.PrimVector @Word8) (V.packR xs)  === xs

    describe "vector pack == packN" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "pack === packN XX" $ \ xs d ->
            (V.pack @V.Vector @Integer xs) === (V.packN d xs)
        prop "pack === packN XX" $ \ xs d ->
            (V.pack @V.PrimVector @Int xs) === (V.packN d xs)
        prop "pack === packN XX" $ \ xs d ->
            (V.pack @V.PrimVector @Word8 xs) === (V.packN d xs)

        prop "packR === packRN XX" $ \ xs d ->
            (V.packR @V.Vector @Integer xs) === (V.packRN d xs)
        prop "packR === packRN XX" $ \ xs d ->
            (V.packR @V.PrimVector @Int xs) === (V.packRN d xs)
        prop "packR === packRN XX" $ \ xs d ->
            (V.packR @V.PrimVector @Word8 xs) === (V.packRN d xs)

    describe "Bytes Hashable instance property" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do

        prop "Vector Word8's hash should be equal to hashWithSalt (Bytes's one) (Bytes's length)" $ \ xs ->
            hash (V.pack @V.Vector @Word8 xs) === hashWithSalt (hash (V.pack @V.PrimVector @Word8 xs)) (List.length xs)
        prop "Vector a's hash should be equal to [a]'s hash" $ \ xs ->
            hash (V.pack @V.Vector @Word8 xs) === hash xs
        prop "Vector a's hash should be equal to [a]'s hash" $ \ xs ->
            hash (V.pack @V.Vector @Int xs) === hash xs
        prop "Vector a's hash should be equal to [a]'s hash" $ \ xs ->
            hash (V.pack @V.Vector @Integer xs) === hash xs


    describe "Bytes IsString instance property" $ do
        prop "ASCII string" $
            "hello world" === V.pack @V.PrimVector (List.map V.c2w "hello world")
        prop "UTF8 string" $
            "你好世界" === V.pack @V.PrimVector (List.map V.c2w "你好世界")

    describe "vector length == List.length" $ do
        prop "vector length === List.length" $ \ xs ->
            (V.length $ V.pack @V.Vector @Integer xs)  ===  List.length xs
        prop "vector length === List.length" $ \ xs ->
            (V.length $ V.pack @V.PrimVector @Int xs)  ===  List.length xs
        prop "vector length === List.length" $ \ xs ->
            (V.length $ V.pack @V.PrimVector @Word8 xs)  ===  List.length xs

    describe "vector append == List.(++)" $ do
        prop "vector eq === List.eq" $ \ xs ys ->
            (V.unpack $ V.pack @V.Vector @Integer xs `V.append` V.pack ys) === (xs ++ ys)
        prop "vector eq === List.eq" $ \ xs ys ->
            (V.unpack $ V.pack @V.PrimVector @Int xs `V.append` V.pack ys) === (xs ++ ys)
        prop "vector eq === List.eq" $ \ xs ys ->
            (V.unpack $ V.pack @V.PrimVector @Word8 xs `V.append` V.pack ys) === (xs ++ ys)

    describe "vector map/map' == List.map" $ do
        prop "vector map === List.map" $ \ xs (Fun _ f) ->
            (V.map @V.Vector @V.Vector (f :: Integer -> Integer) $ V.pack @V.Vector @Integer xs) ===
                (V.pack $ List.map f xs)
        prop "vector map' === List.map" $ \ xs (Fun _ f) ->
            (V.map' @V.Vector @V.Vector (f :: Integer -> Integer) $ V.pack @V.Vector @Integer xs) ===
                (V.pack $ List.map f xs)
        prop "vector map === List.map" $ \ xs (Fun _ f)  ->
            (V.map @V.PrimVector @V.PrimVector (f :: Int -> Word8) $ V.pack @V.PrimVector @Int xs) ===
                (V.pack $ List.map f xs)
        prop "vector map === List.map" $ \ xs (Fun _ f) ->
            (V.map @V.PrimVector @V.PrimVector (f :: Word8 -> Int) $ V.pack @V.PrimVector @Word8 xs) ===
                (V.pack $ List.map f xs)
        prop "vector map === List.map" $ \ xs (Fun _ f) ->
            (V.map @V.PrimVector @V.Vector (f :: Word8 -> Integer) $ V.pack @V.PrimVector @Word8 xs) ===
                (V.pack $ List.map f xs)
        prop "vector map === List.map" $ \ xs (Fun _ f) ->
            (V.map @V.Vector @V.PrimVector (f :: Integer -> Word8) $ V.pack @V.Vector @Integer xs) ===
                (V.pack $ List.map f xs)

    describe "vector imap' (const f) == List.map f" $ do
        prop "vector imap' (const f) == List.map f" $ \ xs (Fun _ f) ->
            (V.imap' @V.Vector @V.PrimVector (const (f :: Integer -> Word8)) $ V.pack @V.Vector @Integer xs) ===
                (V.pack $ List.map f xs)

    describe "vector imap' const == List.zipWith const [0..]" $ do
        prop "vector imap' const == List.zipWith const [0..]" $ \ xs ->
            (V.imap' @V.Vector @V.PrimVector const $ V.pack @V.Vector @Integer xs) ===
                (V.pack $ List.zipWith const [0..] xs)

    describe "vector foldl' == List.foldl'" $ do
        prop "vector foldl' === List.foldl'" $ \ xs f x ->
            (V.foldl' (applyFun2 f :: Integer -> Integer -> Integer) x . V.pack @V.Vector @Integer $ xs)  ===
                (List.foldl' (applyFun2 f) x $ xs)
        prop "vector foldl' x === List.foldl' x" $ \ xs f x ->
            (V.foldl' (applyFun2 f :: Int -> Int -> Int) x . V.pack @V.PrimVector @Int $ xs)  ===
                (List.foldl' (applyFun2 f) x $ xs)
        prop "vector foldl' x === List.foldl' x" $ \ xs f x ->
            (V.foldl' (applyFun2 f :: Int -> Word8 -> Int) x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (List.foldl' (applyFun2 f) x $ xs)

    describe "vector foldr' == List.foldr'" $ do
        prop "vector foldr' === List.foldr" $ \ xs f x ->
            (V.foldr' (applyFun2 f :: Integer -> Integer -> Integer) x . V.pack @V.Vector @Integer $ xs)  ===
                (List.foldr (applyFun2 f) x $ xs)
        prop "vector foldr' x === List.foldr' x" $ \ xs f x ->
            (V.foldr' (applyFun2 f :: Int -> Int -> Int) x . V.pack @V.PrimVector @Int $ xs)  ===
                (List.foldr (applyFun2 f) x $ xs)
        prop "vector foldr' x === List.foldr' x" $ \ xs f x ->
            (V.foldr' (applyFun2 f :: Word8 -> Int -> Int) x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (List.foldr (applyFun2 f) x $ xs)

    describe "vector concat == List.concat" $ do
        prop "vector concat === List.concat" $ \ xss ->
            (V.concat . List.map (V.pack @V.Vector @Integer) $ xss)  === (V.pack . List.concat $ xss)
        prop "vector concat === List.concat" $ \ xss ->
            (V.concat . List.map (V.pack @V.PrimVector @Int) $ xss) === (V.pack . List.concat $ xss)
        prop "vector concat === List.concat" $ \ xss ->
            (V.concat . List.map (V.pack @V.PrimVector @Word8) $ xss) === (V.pack . List.concat $ xss)

    describe "vector concatMap == List.concatMap" $ do
        prop "vector concatMap === List.concatMap" $ \ xss (Fun _ f) ->
            (V.concatMap (V.pack @V.Vector @Integer . f) . V.pack $ xss)  === (V.pack . List.concatMap f $ xss)
        prop "vector concatMap === List.concatMap" $ \ xss (Fun _ f) ->
            (V.concatMap (V.pack @V.PrimVector @Int . f) . V.pack $ xss) === (V.pack . List.concatMap f $ xss)
        prop "vector concatMap === List.concatMap" $ \ xss (Fun _ f) ->
            (V.concatMap (V.pack @V.PrimVector @Word8 . f) . V.pack $ xss) === (V.pack . List.concatMap f $ xss)

    describe "vector all == List.all" $ do
        prop "vector all === List.all" $ \ xs (Fun _ f) ->
            (V.all f . V.pack @V.Vector @Integer $ xs)  === (List.all f $ xs)
        prop "vector all === List.all" $ \ xs (Fun _ f) ->
            (V.all f . V.pack @V.PrimVector @Int $ xs)  === (List.all f $ xs)
        prop "vector all === List.all" $ \ xs (Fun _ f) ->
            (V.all f . V.pack @V.PrimVector @Word8 $ xs)  === (List.all f $ xs)

    describe "vector any == List.any" $ do
        prop "vector any === List.any" $ \ xs (Fun _ f) ->
            (V.any f . V.pack @V.Vector @Integer $ xs)  === (List.any f $ xs)
        prop "vector any === List.any" $ \ xs (Fun _ f) ->
            (V.any f . V.pack @V.PrimVector @Int $ xs)  === (List.any f $ xs)
        prop "vector any === List.any" $ \ xs (Fun _ f) ->
            (V.any f . V.pack @V.PrimVector @Word8 $ xs)  === (List.any f $ xs)

    describe "vector sum == Prelude.sum" $ do
        prop "vector sum === List.sum" $ \ xs ->
            (V.sum . V.pack @V.Vector @Integer $ xs)  === (sum $ xs)
        prop "vector sum === List.sum" $ \ xs ->
            (V.sum . V.pack @V.PrimVector @Int $ xs)  === (sum $ xs)
        prop "vector sum === List.sum" $ \ xs ->
            (V.sum . V.pack @V.PrimVector @Word8 $ xs)  === (sum $ xs)

    describe "vector product == Prelude.product" $ do
        prop "vector product === List.product" $ \ xs ->
            (V.product . V.pack @V.Vector @Integer $ xs)  === (product $ xs)
        prop "vector product === List.product" $ \ xs ->
            (V.product . V.pack @V.PrimVector @Int $ xs)  === (product $ xs)
        prop "vector product === List.product" $ \ xs ->
            (V.product . V.pack @V.PrimVector @Word8 $ xs)  === (product $ xs)

    describe "vector product' == Prelude.product" $ do
        prop "vector product' === List.product" $ \ xs ->
            (V.product' . V.pack @V.Vector @Integer $ xs)  === (product $ xs)
        prop "vector product === List.product" $ \ xs ->
            (V.product' . V.pack @V.PrimVector @Int $ xs)  === (product $ xs)
        prop "vector product === List.product" $ \ xs ->
            (V.product' . V.pack @V.PrimVector @Word8 $ xs)  === (product $ xs)

    describe "vector maximumMaybe == Just . List.maximum" $ do
        prop "vector maximumMaybe === Just . List.maximum" $ \ (NonEmpty xs) ->
            (V.maximumMaybe . V.pack @V.Vector @Integer $ xs)  === (Just . List.maximum $ xs)
        prop "vector maximumMaybe === Just . List.maximum" $ \ (NonEmpty xs) ->
            (V.maximumMaybe . V.pack @V.PrimVector @Int $ xs)  === (Just . List.maximum $ xs)
        prop "vector maximumMaybe === Just . List.maximum" $ \ (NonEmpty xs) ->
            (V.maximumMaybe . V.pack @V.PrimVector @Word8 $ xs)  === (Just . List.maximum $ xs)

    describe "vector minimumMaybe == Just . List.minimum" $ do
        prop "vector minimumMaybe === Just . List.minimum" $ \ (NonEmpty xs) ->
            (V.minimumMaybe . V.pack @V.Vector @Integer $ xs)  === (Just . List.minimum $ xs)
        prop "vector minimumMaybe === Just . List.minimum" $ \ (NonEmpty xs) ->
            (V.minimumMaybe . V.pack @V.PrimVector @Int $ xs)  === (Just . List.minimum $ xs)
        prop "vector minimumMaybe === Just . List.minimum" $ \ (NonEmpty xs) ->
            (V.minimumMaybe . V.pack @V.PrimVector @Word8 $ xs)  === (Just . List.minimum $ xs)

    describe "vector count x == List.length . List.filter (==x)" $ do
        prop "vector count === List.length . List.filter (==x)" $ \ xs x ->
            (V.count x . V.pack @V.Vector @Integer $ xs)  === (List.length . List.filter (==x) $ xs)
        prop "vector count === List.length . List.filter (==x)" $ \ xs x ->
            (V.count x . V.pack @V.PrimVector @Int $ xs)  === (List.length . List.filter (==x) $ xs)
        prop "vector count === List.length . List.filter (==x)" $ \ xs x ->
            (V.count x . V.pack @V.PrimVector @Word8 $ xs)  === (List.length . List.filter (==x) $ xs)

    describe "vector mapAccumL == List.mapAccumL" $ do
        prop "vector mapAccumL === Just . List.mapAccumL" $ \ xs x f ->
            (V.unpack @V.Vector <$> (V.mapAccumL (applyFun2 f) x . V.pack @V.Vector @Integer $ xs))
                === (List.mapAccumL (applyFun2 f) x xs :: (Integer, [Integer]))
        prop "vector mapAccumL === Just . List.mapAccumL" $ \ xs x f ->
            (V.unpack @V.PrimVector <$> (V.mapAccumL (applyFun2 f) x . V.pack @V.PrimVector @Int $ xs))
                === (List.mapAccumL (applyFun2 f) x xs :: (Int, [Int]))
        prop "vector mapAccumL === Just . List.mapAccumL" $ \ xs x f ->
            (V.unpack @V.PrimVector <$> (V.mapAccumL (applyFun2 f) x . V.pack @V.PrimVector @Word8 $ xs))
                === (List.mapAccumL (applyFun2 f) x xs :: (Word8, [Word8]))

    describe "vector mapAccumR == List.mapAccumR" $ do
        prop "vector mapAccumR === Just . List.mapAccumR" $ \ xs x f ->
            (V.unpack @V.Vector <$> (V.mapAccumR (applyFun2 f) x . V.pack @V.Vector @Integer $ xs))
                === (List.mapAccumR (applyFun2 f) x xs :: (Integer, [Integer]))
        prop "vector mapAccumR === Just . List.mapAccumR" $ \ xs x f ->
            (V.unpack @V.PrimVector <$> (V.mapAccumR (applyFun2 f) x . V.pack @V.PrimVector @Int $ xs))
                === (List.mapAccumR (applyFun2 f) x xs :: (Int, [Int]))
        prop "vector mapAccumR === Just . List.mapAccumR" $ \ xs x f ->
            (V.unpack @V.PrimVector <$> (V.mapAccumR (applyFun2 f) x . V.pack @V.PrimVector @Word8 $ xs))
                === (List.mapAccumR (applyFun2 f) x xs :: (Word8, [Word8]))

    describe "vector replicate == List.replicate" $ do
        prop "vector replicate = List.replicate" $ \ n x ->
            (V.unpack . V.replicate @V.Vector @Integer n $ x)  === (List.replicate n $ x)
        prop "vector replicate = List.replicate" $ \ n x ->
            (V.unpack . V.replicate @V.PrimVector @Int n $ x)  === (List.replicate n $ x)
        prop "vector replicate = List.replicate" $ \ n x ->
            (V.unpack . V.replicate @V.PrimVector @Word8 n $ x)  === (List.replicate n $ x)

    describe "vector unfoldrN n == List.take n . List.unfoldr" $ do
        prop "vector unfoldrN n == List.take n . List.unfoldr" $ \ (Fun _ f) (x :: Integer) n ->
            (V.unpack . fst . V.unfoldrN @V.Vector @Integer @Integer n f $ x)
                === (List.take n . List.unfoldr f $ x)
        prop "vector unfoldrN n == List.take n . List.unfoldr" $ \ (Fun _ f) (x :: Int) n ->
            (V.unpack . fst . V.unfoldrN @V.PrimVector @Int @Int n f $ x)
                === (List.take n . List.unfoldr f $ x)
        prop "vector unfoldrN n == List.take n . List.unfoldr" $ \ (Fun _ f) (x :: Word8) n ->
            (V.unpack . fst . V.unfoldrN @V.PrimVector @Word8 @Word8 n f $ x)
                === (List.take n . List.unfoldr f $ x)

    describe "vector elem == List.elem" $ do
        prop "vector elem = List.elem" $ \ y x ->
            (V.elem y . V.pack @V.Vector @Integer $ x)  === (List.elem y $ x)
        prop "vector elem = List.elem" $ \ y x ->
            (V.elem y . V.pack @V.PrimVector @Int $ x)  === (List.elem y $ x)
        prop "vector elem = List.elem" $ \ y x ->
            (V.elem y . V.pack @V.PrimVector @Word8 $ x)  === (List.elem y $ x)

    describe "vector elemIndex == List.elemIndex" $ do
        prop "vector elemIndex = List.elemIndex" $ \ y x ->
            (V.elemIndex y . V.pack @V.Vector @Integer $ x)  === (List.elemIndex y $ x)
        prop "vector elemIndex = List.elemIndex" $ \ y x ->
            (V.elemIndex y . V.pack @V.PrimVector @Int $ x)  === (List.elemIndex y $ x)
        prop "vector elemIndex = List.elemIndex" $ \ y x ->
            (V.elemIndex y . V.pack @V.PrimVector @Word8 $ x)  === (List.elemIndex y $ x)

    describe "vector ByteString roundtrip" $
        prop "vector ByteString roundtrip" $ \ z -> ioProperty $ do
            let bytes = V.pack @V.PrimVector @Word8 z
            bytes' <- FP.bytesFromByteString =<< FP.bytesToByteString bytes
            return $ bytes === bytes'
