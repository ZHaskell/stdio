{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Vector.ExtraSpec where

import qualified Data.List                as List
import           Data.Word
import qualified Std.Data.Vector          as V
import qualified Std.Data.Vector.Base     as V
import qualified Std.Data.Vector.Extra    as V
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec =  describe "vector-extras" $ do
    describe "vector cons == List.(:)" $ do
        prop "vector cons == List.(:)" $ \ xs x ->
            (V.cons x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . (:) x $ xs)
        prop "vector cons == List.(:)" $ \ xs x ->
            (V.cons x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . (:) x $ xs)
        prop "vector cons == List.(:)" $ \ xs x ->
            (V.cons x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . (:) x $ xs)

    describe "vector snoc == List.++" $ do
        prop "vector snoc == List.++" $ \ xs x ->
            ((`V.snoc` x) . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . (++ [x]) $ xs)
        prop "vector snoc == List.++" $ \ xs x ->
            ((`V.snoc` x) . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . (++ [x]) $ xs)
        prop "vector snoc == List.++" $ \ xs x ->
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
        prop "vector take == List.take" $ \ xs x ->
            (V.take x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.take x $ xs)
        prop "vector take == List.take" $ \ xs x ->
            (V.take x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.take x $ xs)
        prop "vector take == List.take" $ \ xs x ->
            (V.take x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.take x $ xs)

    describe "vector takeR x == List.reverse . List.take x . List.reverse" $ do
        prop "vector takeR x == List.reverse . List.take x . List.reverse" $ \ xs x ->
            (V.takeR x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.reverse . List.take x . List.reverse $ xs)
        prop "vector takeR x == List.reverse . List.take x . List.reverse" $ \ xs x ->
            (V.takeR x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.reverse . List.take x . List.reverse $ xs)
        prop "vector takeR x == List.reverse . List.take x . List.reverse" $ \ xs x ->
            (V.takeR x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.reverse . List.take x . List.reverse $ xs)

    describe "vector drop == List.drop" $ do
        prop "vector drop == List.drop" $ \ xs x ->
            (V.drop x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.drop x $ xs)
        prop "vector drop == List.drop" $ \ xs x ->
            (V.drop x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.drop x $ xs)
        prop "vector drop == List.drop" $ \ xs x ->
            (V.drop x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.drop x $ xs)

    describe "vector dropR x == List.reverse . List.drop x . List.reverse" $ do
        prop "vector dropR x == List.reverse . List.drop x . List.reverse" $ \ xs x ->
            (V.dropR x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.reverse . List.drop x . List.reverse $ xs)
        prop "vector dropR x == List.reverse . List.drop x . List.reverse" $ \ xs x ->
            (V.dropR x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.reverse . List.drop x . List.reverse $ xs)
        prop "vector dropR x == List.reverse . List.drop x . List.reverse" $ \ xs x ->
            (V.dropR x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.reverse . List.drop x . List.reverse $ xs)

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

    describe "vector splitAt == List.splitAt" $ do
        prop "vector splitAt == List.splitAt" $ \ xs x ->
            (V.splitAt x . V.pack @V.Vector @Integer $ xs)  ===
                (let (a,b) = List.splitAt x $ xs in (V.pack a, V.pack b))
        prop "vector splitAt == List.splitAt" $ \ xs x ->
            (V.splitAt x . V.pack @V.PrimVector @Int $ xs)  ===
                (let (a,b) = List.splitAt x $ xs in (V.pack a, V.pack b))
        prop "vector splitAt == List.splitAt" $ \ xs x ->
            (V.splitAt x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (let (a,b) = List.splitAt x $ xs in (V.pack a, V.pack b))

    describe "vector takeWhile == List.takeWhile" $ do
        prop "vector takeWhile == List.takeWhile" $ \ xs (Fun _ x) ->
            (V.takeWhile x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.takeWhile x $ xs)
        prop "vector takeWhile == List.takeWhile" $ \ xs (Fun _ x) ->
            (V.takeWhile x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.takeWhile x $ xs)
        prop "vector takeWhile == List.takeWhile" $ \ xs (Fun _ x) ->
            (V.takeWhile x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.takeWhile x $ xs)

    describe "vector takeWhileR == reverse . List.takeWhile . reverse" $ do
        prop "vector takeWhileR == reverse . List.takeWhile . reverse" $ \ xs (Fun _ x) ->
            (V.takeWhileR x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.reverse . List.takeWhile x $ List.reverse xs)
        prop "vector takeWhileR == reverse . List.takeWhile . reverse" $ \ xs (Fun _ x) ->
            (V.takeWhileR x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.reverse . List.takeWhile x $ List.reverse xs)
        prop "vector takeWhileR == reverse . List.takeWhile . reverse" $ \ xs (Fun _ x) ->
            (V.takeWhileR x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.reverse . List.takeWhile x $ List.reverse xs)

    describe "vector dropWhile == List.dropWhile" $ do
        prop "vector dropWhile == List.dropWhile" $ \ xs (Fun _ x) ->
            (V.dropWhile x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.dropWhile x $ xs)
        prop "vector dropWhile == List.dropWhile" $ \ xs (Fun _ x) ->
            (V.dropWhile x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.dropWhile x $ xs)
        prop "vector dropWhile == List.dropWhile" $ \ xs (Fun _ x) ->
            (V.dropWhile x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.dropWhile x $ xs)

    describe "vector dropWhileR == reverse . List.dropWhile . reverse" $ do
        prop "vector dropWhileR == reverse . List.dropWhile . reverse" $ \ xs (Fun _ x) ->
            (V.dropWhileR x . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.reverse . List.dropWhile x $ List.reverse xs)
        prop "vector dropWhileR == reverse . List.dropWhile . reverse" $ \ xs (Fun _ x) ->
            (V.dropWhileR x . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.reverse . List.dropWhile x $ List.reverse xs)
        prop "vector dropWhileR == reverse . List.dropWhile . reverse" $ \ xs (Fun _ x) ->
            (V.dropWhileR x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.reverse . List.dropWhile x $ List.reverse xs)

    describe "vector break == List.break" $ do
        prop "vector break == List.break" $ \ xs (Fun _ x) ->
            (V.break x . V.pack @V.Vector @Integer $ xs)  ===
                (let (a,b) = List.break x $ xs in (V.pack a, V.pack b))
        prop "vector break == List.break" $ \ xs (Fun _ x) ->
            (V.break x . V.pack @V.PrimVector @Int $ xs)  ===
                (let (a,b) = List.break x $ xs in (V.pack a, V.pack b))
        prop "vector break == List.break" $ \ xs (Fun _ x) ->
            (V.break x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (let (a,b) = List.break x $ xs in (V.pack a, V.pack b))

    describe "vector breakOn rules" $ do
        prop "vector breakOn rules" $ \ xs ys ->
            (let (a, b) = V.breakOn (V.pack xs) . V.pack @V.Vector @Integer $ ys
             in (a `V.append` b, V.pack xs `V.isPrefixOf` b || V.null b) === (V.pack ys, True))
        prop "vector breakOn rules" $ \ xs ys ->
            (let (a, b) = V.breakOn (V.pack xs) . V.pack @V.PrimVector @Int $ ys
             in (a `V.append` b, V.pack xs `V.isPrefixOf` b || V.null b) === (V.pack ys, True))
        prop "vector breakOn rules" $ \ xs ys ->
            (let (a, b) = V.breakOn (V.pack xs) . V.pack @V.PrimVector @Word8 $ ys
             in (a `V.append` b, V.pack xs `V.isPrefixOf` b || V.null b) === (V.pack ys, True))

    describe "vector span == List.span" $ do
        prop "vector span == List.span" $ \ xs (Fun _ x) ->
            (V.span x . V.pack @V.Vector @Integer $ xs)  ===
                (let (a,b) = List.span x $ xs in (V.pack a, V.pack b))
        prop "vector span == List.span" $ \ xs (Fun _ x) ->
            (V.span x . V.pack @V.PrimVector @Int $ xs)  ===
                (let (a,b) = List.span x $ xs in (V.pack a, V.pack b))
        prop "vector span == List.span" $ \ xs (Fun _ x) ->
            (V.span x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (let (a,b) = List.span x $ xs in (V.pack a, V.pack b))

    describe "vector breakR == List.break in reverse driection" $ do
        prop "vector breakR == List.break in reverse driection" $ \ xs (Fun _ x) ->
            (V.breakR x . V.pack @V.Vector @Integer $ xs)  ===
                (let (b,a) = List.break x . List.reverse $ xs
                 in (V.reverse $ V.pack a, V.reverse $ V.pack b))
        prop "vector breakR == List.break in reverse driection" $ \ xs (Fun _ x) ->
            (V.breakR x . V.pack @V.PrimVector @Int $ xs)  ===
                (let (b,a) = List.break x . List.reverse $ xs
                 in (V.reverse $ V.pack a, V.reverse $ V.pack b))
        prop "vector breakR == List.break in reverse driection" $ \ xs (Fun _ x) ->
            (V.breakR x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (let (b,a) = List.break x . List.reverse $ xs
                 in (V.reverse $ V.pack a, V.reverse $ V.pack b))

    describe "vector spanR == List.span in reverse driection" $ do
        prop "vector spanR == List.span in reverse driection" $ \ xs (Fun _ x) ->
            (V.spanR x . V.pack @V.Vector @Integer $ xs)  ===
                (let (b,a) = List.span x . List.reverse $ xs
                 in (V.reverse $ V.pack a, V.reverse $ V.pack b))
        prop "vector spanR == List.span in reverse driection" $ \ xs (Fun _ x) ->
            (V.spanR x . V.pack @V.PrimVector @Int $ xs)  ===
                (let (b,a) = List.span x . List.reverse $ xs
                 in (V.reverse $ V.pack a, V.reverse $ V.pack b))
        prop "vector spanR == List.span in reverse driection" $ \ xs (Fun _ x) ->
            (V.spanR x . V.pack @V.PrimVector @Word8 $ xs)  ===
                (let (b,a) = List.span x . List.reverse $ xs
                 in (V.reverse $ V.pack a, V.reverse $ V.pack b))

    describe "vector group == List.group" $ do
        prop "vector group == List.group" $ \ xs ->
            (V.group . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack <$> List.group xs)
        prop "vector group == List.group" $ \ xs ->
            (V.group . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack <$> List.group xs)
        prop "vector group == List.group" $ \ xs ->
            (V.group . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack <$> List.group xs)

    describe "vector groupBy == List.groupBy" $ do
        prop "vector groupBy == List.groupBy" $ \ xs x ->
            (V.groupBy (applyFun2 x) . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack <$> List.groupBy (applyFun2 x) xs)
        prop "vector groupBy == List.groupBy" $ \ xs x ->
            (V.groupBy (applyFun2 x) . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack <$> List.groupBy (applyFun2 x) xs)
        prop "vector groupBy == List.groupBy" $ \ xs x ->
            (V.groupBy (applyFun2 x) . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack <$> List.groupBy (applyFun2 x) xs)

    describe "vector stripPrefix a (a+b) = b " $ do
        prop "vector stripPrefix == List.stripPrefix" $ \ xs ys ->
            (V.stripPrefix (V.pack xs) . V.pack @V.Vector @Integer $ xs++ys) ===
                (Just $ V.pack ys)
        prop "vector stripPrefix == List.stripPrefix" $ \ xs ys ->
            (V.stripPrefix (V.pack xs) . V.pack @V.PrimVector @Int $ xs++ys) ===
                (Just $ V.pack ys)
        prop "vector stripPrefix == List.stripPrefix" $ \ xs ys ->
            (V.stripPrefix (V.pack xs) . V.pack @V.PrimVector @Word8 $ xs++ys) ===
                (Just $ V.pack ys)

    describe "vector stripSuffix b (a+b) = a " $ do
        prop "vector stripSuffix == List.stripSuffix" $ \ xs ys ->
            (V.stripSuffix (V.pack xs) . V.pack @V.Vector @Integer $ ys++xs) ===
                (Just $ V.pack ys)
        prop "vector stripSuffix == List.stripSuffix" $ \ xs ys ->
            (V.stripSuffix (V.pack xs) . V.pack @V.PrimVector @Int $ ys++xs) ===
                (Just $ V.pack ys)
        prop "vector stripSuffix == List.stripSuffix" $ \ xs ys ->
            (V.stripSuffix (V.pack xs) . V.pack @V.PrimVector @Word8 $ ys++xs) ===
                (Just $ V.pack ys)

    describe "vector isInfixOf b (a+b+c) = True " $ do
        prop "vector isInfixOf == List.isInfixOf" $ \ xs ys zs ->
            (V.isInfixOf (V.pack xs) . V.pack @V.Vector @Integer $ ys++xs++zs) === True
        prop "vector isInfixOf == List.isInfixOf" $ \ xs ys zs ->
            (V.isInfixOf (V.pack xs) . V.pack @V.PrimVector @Int $ ys++xs++zs) === True
        prop "vector isInfixOf == List.isInfixOf" $ \ xs ys zs ->
            (V.isInfixOf (V.pack xs) . V.pack @V.PrimVector @Word8 $ ys++xs++zs) === True

    describe "let (c,a,b) = vector commonPrefix x y in (a,b) = (stripPrefix c x,stripPrefix c y) " $ do
        prop "vector commonPrefix rules" $ \ xs ys ->
            let (c,a,b) = V.commonPrefix (V.pack xs) . V.pack @V.Vector @Integer $ ys
                Just xs' = V.stripPrefix c $ V.pack xs
                Just ys' = V.stripPrefix c $ V.pack ys
            in (a,b) === (xs', ys')
        prop "vector commonPrefix rules" $ \ xs ys ->
            let (c,a,b) = V.commonPrefix (V.pack xs) . V.pack @V.PrimVector @Int $ ys
                Just xs' = V.stripPrefix c $ V.pack xs
                Just ys' = V.stripPrefix c $ V.pack ys
            in (a,b) === (xs', ys')
        prop "vector commonPrefix rules" $ \ xs ys ->
            let (c,a,b) = V.commonPrefix (V.pack xs) . V.pack @V.PrimVector @Word8 $ ys
                Just xs' = V.stripPrefix c $ V.pack xs
                Just ys' = V.stripPrefix c $ V.pack ys
            in (a,b) === (xs', ys')

    describe "vector intercalate [x] . split x == id" $ do
        prop "vector intercalate [x] . split x == id" $ \ xs x ->
            (V.intercalate (V.singleton x) . V.split x . V.pack @V.Vector @Integer $ xs) ===
                V.pack xs
        prop "vector intercalate [x] . split x == id" $ \ xs x ->
            (V.intercalate (V.singleton x) . V.split x . V.pack @V.PrimVector @Int $ xs) ===
                V.pack xs
        prop "vector intercalate [x] . split x == id" $ \ xs x ->
            (V.intercalate (V.singleton x) . V.split x . V.pack @V.PrimVector @Word8 $ xs) ===
                V.pack xs

    describe "vector intercalate x . splitOn x == id" $ do
        prop "vector intercalate x . splitOn x == id" $ \ xs x ->
            (V.intercalate (V.pack x) . V.splitOn (V.pack x) . V.pack @V.Vector @Integer $ xs) ===
                V.pack xs
        prop "vector intercalate x . splitOn x == id" $ \ xs x ->
            (V.intercalate (V.pack x) . V.splitOn (V.pack x) . V.pack @V.PrimVector @Int $ xs) ===
                V.pack xs
        prop "vector intercalate x . splitOn x == id" $ \ xs x ->
            (V.intercalate (V.pack x) . V.splitOn (V.pack x) . V.pack @V.PrimVector @Word8 $ xs) ===
                V.pack xs

    describe "vector words == List.words" $ do
        prop "vector words === List.words" $ \ xs ->
            (V.words . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.map V.c2w <$> (List.words . List.map V.w2c $ xs))

    describe "vector lines == List.lines" $ do
        prop "vector lines === List.lines" $ \ xs ->
            (V.lines . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.map V.c2w <$> (List.lines . List.map V.w2c $ xs))

    describe "vector unwords == List.unwords" $ do
        prop "vector unwords === List.unwords" $ \ xs ->
            (V.unwords $ V.pack @V.PrimVector @Word8 <$> xs)  ===
                (V.pack (List.map V.c2w . List.unwords $ List.map V.w2c <$> xs))

    describe "vector unlines == List.unlines" $ do
        prop "vector unlines === List.unlines" $ \ xs ->
            (V.unlines $ V.pack @V.PrimVector @Word8 <$> xs)  ===
                (V.pack (List.map V.c2w . List.unlines $ List.map V.w2c <$> xs))

    describe "vector padLeft n x xs = if l >= n then xs else replicate (n-l) x ++ xs" $ do
        prop "vector padLeft n x xs = if l >= n then xs else replicate (n-l) x ++ xs" $ \ xs n x ->
            (V.padLeft n x . V.pack @V.Vector @Integer $ xs) ===
                (let l = List.length xs
                 in if l >= n then V.pack xs
                              else V.pack $ (List.replicate (n-l) x ++ xs))
        prop "vector padLeft n x xs = if l >= n then xs else replicate (n-l) x ++ xs" $ \ xs n x ->
            (V.padLeft n x . V.pack @V.PrimVector @Int $ xs) ===
                (let l = List.length xs
                 in if l >= n then V.pack xs
                              else V.pack $ (List.replicate (n-l) x ++ xs))
        prop "vector padLeft n x xs = if l >= n then xs else replicate (n-l) x ++ xs" $ \ xs n x ->
            (V.padLeft n x . V.pack @V.PrimVector @Word8 $ xs) ===
                (let l = List.length xs
                 in if l >= n then V.pack xs
                              else V.pack $ (List.replicate (n-l) x ++ xs))

    describe "vector padRight n x xs = if l >= n then xs else xs ++ List.replicate (n-l) x" $ do
        prop "vector padRight n x xs = if l >= n then xs else xs ++ List.replicate (n-l) x" $ \ xs n x ->
            (V.padRight n x . V.pack @V.Vector @Integer $ xs) ===
                (let l = List.length xs
                 in if l >= n then V.pack xs
                              else V.pack $ xs ++ (List.replicate (n-l) x))
        prop "vector padRight n x xs = if l >= n then xs else xs ++ List.replicate (n-l) x" $ \ xs n x ->
            (V.padRight n x . V.pack @V.PrimVector @Int $ xs) ===
                (let l = List.length xs
                 in if l >= n then V.pack xs
                              else V.pack $ xs ++ (List.replicate (n-l) x))
        prop "vector padRight n x xs = if l >= n then xs else xs ++ List.replicate (n-l) x" $ \ xs n x ->
            (V.padRight n x . V.pack @V.PrimVector @Word8 $ xs) ===
                (let l = List.length xs
                 in if l >= n then V.pack xs
                              else V.pack $ xs ++ (List.replicate (n-l) x))

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

    describe "vector intercalateElem x == List.intercalate [x]" $ do
        prop "vector intercalateElem x === List.intercalate [x]" $ \ xs x ->
            (V.intercalateElem x . List.map (V.pack @V.Vector @Integer) $ xs)  ===
                (V.pack . List.intercalate [x] $ xs)
        prop "vector intercalateElem ys === List.intercalate x" $ \ xs x ->
            (V.intercalateElem x . List.map (V.pack @V.PrimVector @Int) $ xs)  ===
                (V.pack . List.intercalate [x] $ xs)
        prop "vector intercalateElem ys === List.intercalate x" $ \ xs x ->
            (V.intercalateElem x . List.map (V.pack @V.PrimVector @Word8) $ xs)  ===
                (V.pack . List.intercalate [x] $ xs)

    describe "vector transpose == List.transpose" $ do
        prop "vector transpose == List.transpose" $ \ xs ->
            (V.transpose $ V.pack @V.Vector @Integer <$> xs)  ===
                (V.pack <$> List.transpose xs)
        prop "vector transpose == List.transpose" $ \ xs ->
            (V.transpose $ V.pack @V.PrimVector @Int <$> xs)  ===
                (V.pack <$> List.transpose xs)
        prop "vector transpose == List.transpose" $ \ xs ->
            (V.transpose $ V.pack @V.PrimVector @Word8 <$> xs)  ===
                (V.pack <$> List.transpose xs)

    describe "vector zipWith' == List.zipWith" $ do
        prop "vector zipWith' == List.zipWith" $ \ xs ys x ->
            let pack' = V.pack @V.Vector @Integer
            in (V.zipWith' (applyFun2 x) (pack' xs) (pack' ys))  ===
                (pack' $ List.zipWith (applyFun2 x) xs ys)
        prop "vector zipWith == List.zipWith" $ \ xs ys x ->
            let pack' = V.pack @V.PrimVector @Int
            in (V.zipWith' (applyFun2 x) (pack' xs) (pack' ys))  ===
                (pack' $ List.zipWith (applyFun2 x) xs ys)
        prop "vector zipWith' == List.zipWith" $ \ xs ys x ->
            let pack' = V.pack @V.PrimVector @Word8
            in (V.zipWith' (applyFun2 x) (pack' xs) (pack' ys))  ===
                (pack' $ List.zipWith (applyFun2 x) xs ys)

    describe "vector unzipWith' f == List.unzip . List.map f" $ do
        prop "vector zipWith' == List.unzip . List.map f" $ \ zs (Fun _ x) ->
            let pack' = V.pack @V.Vector @Integer
            in (V.unzipWith' x (pack' zs))  ===
                (let (a,b) = List.unzip (List.map x zs) in (pack' a, pack' b))
        prop "vector zipWith == List.unzip . List.map f" $ \ zs (Fun _ x) ->
            let pack' = V.pack @V.PrimVector @Int
            in (V.unzipWith' x (pack' zs))  ===
                (let (a,b) = List.unzip (List.map x zs) in (pack' a, pack' b))
        prop "vector zipWith' == List.unzip . List.map f" $ \ zs (Fun _ x) ->
            let pack' = V.pack @V.PrimVector @Word8
            in (V.unzipWith' x (pack' zs))  ===
                (let (a,b) = List.unzip (List.map x zs) in (pack' a, pack' b))

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

    describe "vector scanl1' == List.scanl1" $ do
        prop "vector scanl1' === List.scanl1" $ \ xs f ->
            (V.scanl1' (applyFun2 f :: Integer -> Integer -> Integer) . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.scanl1 (applyFun2 f) $ xs)
        prop "vector scanl1' x === List.scanl1 x" $ \ xs f ->
            (V.scanl1' (applyFun2 f :: Int -> Int -> Int) . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.scanl1 (applyFun2 f) $ xs)
        prop "vector scanl1' x === List.scanl1 x" $ \ xs f ->
            (V.scanl1' (applyFun2 f :: Word8 -> Word8 -> Word8) . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.scanl1 (applyFun2 f) $ xs)

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

    describe "vector scanr1' == List.scanr1" $ do
        prop "vector scanr1' === List.scanr1" $ \ xs f ->
            (V.scanr1' (applyFun2 f :: Integer -> Integer -> Integer) . V.pack @V.Vector @Integer $ xs)  ===
                (V.pack . List.scanr1 (applyFun2 f) $ xs)
        prop "vector scanr1' x === List.scanr1 x" $ \ xs f ->
            (V.scanr1' (applyFun2 f :: Int -> Int -> Int) . V.pack @V.PrimVector @Int $ xs)  ===
                (V.pack . List.scanr1 (applyFun2 f) $ xs)
        prop "vector scanr1' x === List.scanr1 x" $ \ xs f ->
            (V.scanr1' (applyFun2 f :: Word8 -> Word8 -> Word8) . V.pack @V.PrimVector @Word8 $ xs)  ===
                (V.pack . List.scanr1 (applyFun2 f) $ xs)
