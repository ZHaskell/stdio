{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Std.Data.LEONSpec where

import qualified Data.List                as List
import           Data.Word
import           Data.Int
import           GHC.Natural
import qualified Std.Data.Builder         as B
import qualified Std.Data.Parser          as P
import qualified Std.Data.CBytes          as CB
import qualified Std.Data.Text            as T
import qualified Std.Data.Vector.Base     as V
import qualified Std.Data.LEON            as LEON
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

data Test1 = Test1 Int8 Int16 Int32 Int64 Int Word8 Word16 Word32 Word64 Word
    deriving (Generic, LEON.LEON, Eq, Show)

data Test2 = Test2 (LEON.BE Int16) (LEON.BE Word32) (LEON.BE Int64) (LEON.BE Word)
    deriving (Generic, LEON.LEON, Eq, Show)

data Test3 = Test3Integer Integer | Test3Natural Natural
    deriving (Generic, LEON.LEON, Eq, Show)

data Test4 = Test4 [Integer]
    deriving (Generic, LEON.LEON, Eq, Show)

data Test5 = Test5 Ordering Bool
    deriving (Generic, LEON.LEON, Eq, Show)

data Test6 = Test6 (V.Vector Integer) V.Bytes (V.PrimVector Int) T.Text CB.CBytes
    deriving (Generic, LEON.LEON, Eq, Show)

spec :: Spec
spec = describe "LEON instance roundtrip" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
    prop "Test1 roundtrip" $ \ a b c d e f g h i j ->
        let t = Test1 a b c d e f g h i j
        in P.parse LEON.decode (B.buildBytes $ LEON.encode t) === Right t

    prop "Test2 roundtrip" $ \ a b c d  ->
        let t = Test2 (LEON.BE a) (LEON.BE b) (LEON.BE c) (LEON.BE d)
        in P.parse LEON.decode (B.buildBytes $ LEON.encode t) === Right t

    prop "Test3 roundtrip" $ \ a b (Positive c) ->
        let t = if a then Test3Integer b else Test3Natural (fromIntegral (c :: Integer))
        in P.parse LEON.decode (B.buildBytes $ LEON.encode t) === Right t

    prop "Test4 roundtrip" $ \ xs ->
        let t = Test4 xs
        in P.parse LEON.decode (B.buildBytes $ LEON.encode t) === Right t

    prop "Test5 roundtrip" $ \ a b ->
        let t = Test5 a b
        in P.parse LEON.decode (B.buildBytes $ LEON.encode t) === Right t

    prop "Test6 roundtrip" $ \ xs ys zs ts bs ->
        let t = Test6 (V.pack xs) (V.pack ys) (V.pack zs) (T.pack ts) (CB.pack bs)
        in P.parse LEON.decode (B.buildBytes $ LEON.encode t) === Right t
