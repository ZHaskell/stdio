{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.Parser.BaseSpec where

import qualified Data.List                as L
import           Data.Word
import           Data.Int
import           GHC.Float
import           Text.Printf                 (printf)
import           Data.Word8                  (toLower, toUpper)
import qualified Std.Data.Parser.Base    as P
import qualified Std.Data.Text as T
import qualified Std.Data.Vector.Base as V
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck


parse' :: P.Parser a -> [Word8] -> Maybe a
parse' p str = case P.parse p (V.pack str) of
    Left msg -> Nothing
    Right a  -> Just a

parse'' :: P.Parser a -> [Word8] -> Maybe (V.Bytes, a)
parse'' p str = case P.parse' p (V.pack str) of
    (rest, Right a)  -> Just (rest, a)
    _                -> Nothing

spec :: Spec
spec = describe "parsers" . modifyMaxSuccess (*10) . modifyMaxSize (*10)  $ do
        prop "satisfy" $ \ w s ->
            parse' (P.satisfy (<=w)) (w:s) === Just w

        prop "satisfyWith" $ \ w s (Fun _ f) ->
            parse' (P.satisfyWith f (== f w)) (w:s) === Just (f w :: Int)

        prop "word8" $ \ w w' s ->
            parse' (P.word8 w) (w':s) ===
                (if w == w' then Just () else Nothing)

        prop "skipWhile" $ \ s (Fun _ f) ->
            parse'' (P.skipWhile f) s === Just (V.pack (L.dropWhile f s), ())

        prop "takeWhile" $ \ s (Fun _ f) ->
            parse'' (P.takeWhile f) s === Just (V.pack (L.dropWhile f s), V.pack (L.takeWhile f s))

        prop "takeTill" $ \ s (Fun _ f) ->
            let (s1, s2) = L.break f s
            in parse'' (P.takeTill f) s === Just (V.pack s2, V.pack s1)

        prop "takeWhile1" $ \ s (Fun _ f) ->
            parse'' (P.takeWhile1 f) s ===
                case s of
                    (w:_) | f w  -> Just (V.pack (L.dropWhile f s), V.pack (L.takeWhile f s))
                    _            -> Nothing

        prop "take" $ \ s n ->
            parse'' (P.take n) s ===
                if L.length s >= n
                    then Just (V.pack (L.drop n s), V.pack (L.take n s))
                    else Nothing

        prop "skip" $ \ s n ->
            parse'' (P.skip n) s ===
                if L.length s >= n
                    then Just (V.pack (L.drop n s), ())
                    else Nothing

        prop "anyWord8" $ \ s ->
            parse' ((,) <$> P.anyWord8 <*> P.takeWhile (const True)) s ===
                case s of [] -> Nothing
                          (w:s') -> Just (w, V.pack s')


        prop "peek" $ \ s ->
            parse' ((,) <$> P.peek <*> P.takeWhile (const True)) s ===
                case s of [] -> Nothing
                          (w:_) -> Just (w, V.pack s)

        prop "peekMaybe" $ \ s ->
            parse' ((,) <$> P.peekMaybe <*> P.takeWhile (const True)) s ===
                case s of [] -> Just (Nothing, V.pack s)
                          (w:_) -> Just (Just w, V.pack s)

        prop "bytes" $ \ s t ->
            parse' (P.bytes . V.pack $ t) s ===
                if L.take (L.length t) s == t then Just () else Nothing

        prop "bytes" $ \ s t ->
            parse'' (P.bytes . V.pack $ t) (t ++ s) === Just (V.pack s, ())

        prop "bytes" $ \ s t u ->
            parse'' (P.bytes (V.pack s) >> P.bytes (V.pack t)) (s ++ t ++ u) === Just (V.pack u, ())

        prop "bytesCI" $ \ s t ->
            parse'' (P.bytesCI . V.pack $ t) (t ++ s) === Just (V.pack s, ())

        prop "bytesCI" $ \ s t ->
            parse'' (P.bytesCI . V.pack $ t) (L.map toLower t ++ s) === Just (V.pack s, ())

        prop "endOfInput" $ \ s ->
            parse' P.endOfInput s ===
                case s of [] -> Just True
                          _  -> Just False

        prop "scan" $ \ s l ->
            let go l  _ | l <= 0    = Nothing
                        | otherwise = Just (l-1)
            in parse' (P.scan l go) s === Just (V.pack $ L.take l s)

        prop "endOfLine" $ \ s ->
            let r = fromIntegral (fromEnum '\r')
                n =  fromIntegral (fromEnum '\n')
            in parse'' (P.skipWhile (\w -> w `L.notElem` [r, n]) >> P.endOfLine) s ===
                    case break (\w -> w `L.elem` [r, n]) s of
                        (_, bs) -> case bs of
                            (b:bs') | b == n -> Just (V.pack bs', ())
                            (b:c:bs') | b == r && c == n -> Just (V.pack bs', ())
                            _ -> Nothing

