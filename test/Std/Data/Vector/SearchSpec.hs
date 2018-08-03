{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Vector.SearchSpec where

import qualified Data.List                as List
import           Data.Word
import qualified Std.Data.Vector          as V
import qualified Std.Data.Vector.Search   as V
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "vector search" $ do
    describe "snd . vector find == List.find" $ do
        prop "snd .vector find = List.find" $ \ (Fun _ y) x ->
            (snd . V.find y . V.pack @V.Vector @Integer $ x)  === (List.find y $ x)
        prop "snd . vector find = List.find" $ \ (Fun _ y) x ->
            (snd . V.find y . V.pack @V.PrimVector @Int $ x)  === (List.find y $ x)
        prop "snd . vector find = List.find" $ \ (Fun _ y) x ->
            (snd . V.find y . V.pack @V.PrimVector @Word8 $ x)  === (List.find y $ x)

    describe "vector findIndexOrEnd == maybe List.length List.findIndexOrEnd" $ do
        prop "vector findIndexOrEnd = maybe List.length List.findIndexOrEnd" $ \ (Fun _ y) x ->
            (V.findIndexOrEnd y . V.pack @V.Vector @Integer $ x)  ===
                (maybe (List.length x) id $ List.findIndex y x)
        prop "vector findIndexOrEnd = maybe List.length List.findIndexOrEnd" $ \ (Fun _ y) x ->
            (V.findIndexOrEnd y . V.pack @V.PrimVector @Int $ x)  ===
                (maybe (List.length x) id $ List.findIndex y x)
        prop "vector findIndexOrEnd = maybe List.length List.findIndexOrEnd" $ \ (Fun _ y) x ->
            (V.findIndexOrEnd y . V.pack @V.PrimVector @Word8 $ x)  ===
                (maybe (List.length x) id $ List.findIndex y x)

    describe "vector findIndexOrEnd ==  length - findLastIndexOrStart . reverse - 1" $ do
        prop "vector findIndexOrEnd = length - findLastIndexOrStart . reverse - 1" $ \ (Fun _ y) x ->
            (V.findIndexOrEnd y . V.pack @V.Vector @Integer $ x)  ===
                (List.length x - 1 - (V.findLastIndexOrStart y . V.reverse . V.pack @V.Vector @Integer $ x))
        prop "vector findIndexOrEnd = length - findLastIndexOrStart . reverse - 1" $ \ (Fun _ y) x ->
            (V.findIndexOrEnd y . V.pack @V.PrimVector @Int $ x)  ===
                (List.length x - 1 - (V.findLastIndexOrStart y . V.reverse . V.pack @V.PrimVector @Int $ x))
        prop "vector findIndexOrEnd = length - findLastIndexOrStart . reverse - 1" $ \ (Fun _ y) x ->
            (V.findIndexOrEnd y . V.pack @V.PrimVector @Word8 $ x)  ===
                (List.length x - 1 - (V.findLastIndexOrStart y . V.reverse . V.pack @V.PrimVector @Word8 $ x))

    describe "vector elemIndices == List.elemIndices" $ do
        prop "vector elemIndices = List.elemIndices" $ \ y x ->
            (V.elemIndices y . V.pack @V.Vector @Integer $ x)  === (List.elemIndices y $ x)
        prop "vector elemIndices = List.elemIndices" $ \ y x ->
            (V.elemIndices y . V.pack @V.PrimVector @Int $ x)  === (List.elemIndices y $ x)
        prop "vector elemIndices = List.elemIndices" $ \ y x ->
            (V.elemIndices y . V.pack @V.PrimVector @Word8 $ x)  === (List.elemIndices y $ x)

    describe "vector filter == List.filter" $ do
        prop "vector filter = List.filter" $ \ (Fun _ y) x ->
            (V.filter y . V.pack @V.Vector @Integer $ x)  === (V.pack . List.filter y $ x)
        prop "vector filter = List.filter" $ \ (Fun _ y) x ->
            (V.filter y . V.pack @V.PrimVector @Int $ x)  === (V.pack . List.filter y $ x)
        prop "vector filter = List.filter" $ \ (Fun _ y) x ->
            (V.filter y . V.pack @V.PrimVector @Word8 $ x)  === (V.pack . List.filter y $ x)

    describe "vector partition == List.partition" $ do
        prop "vector partition = List.partition" $ \ (Fun _ y) x ->
            (V.partition y . V.pack @V.Vector @Integer $ x)  ===
                (let (a,b) = List.partition y $ x in (V.pack a, V.pack b))
        prop "vector partition = List.partition" $ \ (Fun _ y) x ->
            (V.partition y . V.pack @V.PrimVector @Int $ x)  ===
                (let (a,b) = List.partition y $ x in (V.pack a, V.pack b))
        prop "vector partition = List.partition" $ \ (Fun _ y) x ->
            (V.partition y . V.pack @V.PrimVector @Word8 $ x)  ===
                (let (a,b) = List.partition y $ x in (V.pack a, V.pack b))

    describe "vector indices property" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "subvector at indices should be equal to needle" $ \ needle haystack ->
            (let is = V.indices (V.pack needle) (V.pack @V.Vector @Integer $ haystack) False
             in all (\i -> List.take (List.length needle) (List.drop i haystack) == needle) is
            ) === True
        prop "subvector at indices should be equal to needle" $ \ needle haystack ->
            (let is = V.indices (V.pack needle) (V.pack @V.PrimVector @Int $ haystack) False
             in all (\i -> List.take (List.length needle) (List.drop i haystack) == needle) is
            ) === True
        prop "subvector at indices should be equal to needle" $ \ needle haystack ->
            (let is = V.indices (V.pack needle) (V.pack @V.PrimVector @Word8 $ haystack) False
             in all (\i -> List.take (List.length needle) (List.drop i haystack) == needle) is
            ) === True

    describe "vector indices property" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "indices should not overlapped" $ \ needle haystack ->
            (let is = V.indices (V.pack needle) (V.pack @V.Vector @Integer $ haystack) False
                 is' = drop 1 is
                 isDiff = List.zipWith (-) is' is
             in all (>= List.length needle)  isDiff
            ) === True
        prop "indices should not overlapped" $ \ needle haystack ->
            (let is = V.indices (V.pack needle) (V.pack @V.PrimVector @Int $ haystack) False
                 is' = drop 1 is
                 isDiff = List.zipWith (-) is' is
             in all (>= List.length needle)  isDiff
            ) === True
        prop "indices should not overlapped" $ \ needle haystack ->
            (let is = V.indices (V.pack needle) (V.pack @V.PrimVector @Word8 $ haystack) False
                 is' = drop 1 is
                 isDiff = List.zipWith (-) is' is
             in all (>= List.length needle)  isDiff
            ) === True

    describe "vector indices property (partial match)" $ do
        prop "subvector at indices should be equal to needle" $ \ needle haystack ->
            (let is = V.indices (V.pack needle) (V.pack @V.Vector @Integer $ haystack) True
             in all (\i ->
                    if (i > 0)
                    then List.take (List.length needle) (List.drop i haystack) == needle
                    else (List.drop (List.length haystack + i) haystack) ==
                            (List.take (0-i) needle)
                ) is
            ) === True
        prop "subvector at indices should be equal to needle" $ \ needle haystack ->
            (let is = V.indices (V.pack needle) (V.pack @V.PrimVector @Int $ haystack) True
             in all (\i ->
                    if (i > 0)
                    then List.take (List.length needle) (List.drop i haystack) == needle
                    else (List.drop (List.length haystack + i) haystack) ==
                            (List.take (0-i) needle)
                ) is
            ) === True
        prop "subvector at indices should be equal to needle" $ \ needle haystack ->
            (let is = V.indices (V.pack needle) (V.pack @V.PrimVector @Word8 $ haystack) True
             in all (\i ->
                    if (i > 0)
                    then List.take (List.length needle) (List.drop i haystack) == needle
                    else (List.drop (List.length haystack + i) haystack) ==
                            (List.take (0-i) needle)
                ) is
            ) === True

    describe "vector overlapping indices property" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "subvector at indices should be equal to needle" $ \ needle haystack ->
            (let is = V.indicesOverlapping (V.pack needle) (V.pack @V.Vector @Integer $ haystack) False
             in all (\i -> List.take (List.length needle) (List.drop i haystack) == needle) is
            ) === True
        prop "subvector not at indicesOverlapping should be not equal to needle" $ \ needle haystack  ->
            (let is = V.indicesOverlapping (V.pack needle) (V.pack @V.Vector @Integer $ haystack) False
                 is' = filter (`notElem` is) [0..List.length haystack-1]
             in all (\i -> List.take (List.length needle) (List.drop i haystack) /= needle) is'
            ) === True
        prop "subvector at indicesOverlapping should be equal to needle" $ \ needle haystack ->
            (let is = V.indicesOverlapping (V.pack needle) (V.pack @V.PrimVector @Int $ haystack) False
             in all (\i -> List.take (List.length needle) (List.drop i haystack) == needle) is
            ) === True
        prop "subvector not at indicesOverlapping should be not equal to needle" $ \ needle haystack  ->
            (let is = V.indicesOverlapping (V.pack needle) (V.pack @V.PrimVector @Int $ haystack) False
                 is' = filter (`notElem` is) [0..List.length haystack-1]
             in all (\i -> List.take (List.length needle) (List.drop i haystack) /= needle) is'
            ) === True
        prop "subvector at indicesOverlapping should be equal to needle" $ \ needle haystack ->
            (let is = V.indicesOverlapping (V.pack needle) (V.pack @V.PrimVector @Word8 $ haystack) False
             in all (\i -> List.take (List.length needle) (List.drop i haystack) == needle) is
            ) === True
        prop "subvector not at indicesOverlapping should be not equal to needle" $ \ needle haystack  ->
            (let is = V.indicesOverlapping (V.pack needle) (V.pack @V.PrimVector @Word8 $ haystack) False
                 is' = filter (`notElem` is) [0..List.length haystack-1]
             in all (\i -> List.take (List.length needle) (List.drop i haystack) /= needle) is'
            ) === True

    describe "vector indicesOverlapping property (partial match)" $ do
        prop "subvector at indicesOverlapping should be equal to needle" $ \ needle haystack ->
            (let is = V.indicesOverlapping (V.pack needle) (V.pack @V.Vector @Integer $ haystack) True
             in all (\i ->
                    if (i > 0)
                    then List.take (List.length needle) (List.drop i haystack) == needle
                    else (List.drop (List.length haystack + i) haystack) ==
                            (List.take (0-i) needle)
                ) is
            ) === True
        prop "subvector at indicesOverlapping should be equal to needle" $ \ needle haystack ->
            (let is = V.indicesOverlapping (V.pack needle) (V.pack @V.PrimVector @Int $ haystack) True
             in all (\i ->
                    if (i > 0)
                    then List.take (List.length needle) (List.drop i haystack) == needle
                    else (List.drop (List.length haystack + i) haystack) ==
                            (List.take (0-i) needle)
                ) is
            ) === True
        prop "subvector at indicesOverlapping should be equal to needle" $ \ needle haystack ->
            (let is = V.indicesOverlapping (V.pack needle) (V.pack @V.PrimVector @Word8 $ haystack) True
             in all (\i ->
                    if (i > 0)
                    then List.take (List.length needle) (List.drop i haystack) == needle
                    else (List.drop (List.length haystack + i) haystack) ==
                            (List.take (0-i) needle)
                ) is
            ) === True
