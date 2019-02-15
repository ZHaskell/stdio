{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

-- | Lot of benchmarks here use (\ x -> foo x) instead of foo
-- because ghc inliner only inline foo (hence specialize it) when it's saturated

module Bytes (bytes) where

import Criterion.Main
import qualified Data.ByteString as B
import qualified Std.Data.Vector as V
import qualified Data.List as List
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Control.DeepSeq
import Builder

import Prelude hiding (reverse,head,tail,last,init,null
    ,length,map,lines,foldl,foldr,unlines
    ,concat,any,take,drop,splitAt,takeWhile
    ,dropWhile,span,break,elem,filter,maximum
    ,minimum,all,concatMap,foldl1,foldr1
    ,scanl1,scanl,scanr,scanr1
    ,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn,interact
    ,zip,zipWith,unzip,notElem
    )

list100, list1000, list10000 :: [Word8]
list100 = List.replicate 100 127
list1000 = List.replicate 1000 127
list10000 = List.replicate 10000 127


vector100, vector1000, vector10000 :: VU.Vector Word8
vector100 = VU.fromList list100
vector1000 = VU.fromList list1000
vector10000 = VU.fromList list10000

bytes100, bytes1000, bytes10000 :: V.Bytes
bytes100 = V.pack list100
bytes1000 = V.pack list1000
bytes10000 = V.pack list10000

bytestring100, bytestring1000, bytestring10000 :: B.ByteString
bytestring100 = B.pack list100
bytestring1000 = B.pack list1000
bytestring10000 = B.pack list10000

wordZ :: Word8
wordZ = 0

bytes :: [Benchmark]
bytes = -- List.reverse
    [ bgroup "singleton" singleton
    , bgroup "map" map
    , bgroup "eq" eq
    , bgroup "pack/100 elems"  packSmall
    , bgroup "pack/10000 elems"  packLarge
    , bgroup "unpack" unpack
    , bgroup "map" map
    , bgroup "reverse" reverse
    , bgroup "intersperse" intersperse
    , bgroup "intercalate" intercalate
    , bgroup "foldl" foldl
    , bgroup "foldl'" foldl'
    , bgroup "foldr" foldr
    , bgroup "foldr'" foldr'
    , bgroup "concat" concat
    , bgroup "concatMap" concatMap
    , bgroup "all" all
    , bgroup "any" any
    , bgroup "mapAccumL" mapAccumL
    , bgroup "mapAccumR" mapAccumR
    , bgroup "replicate" replicate
    , bgroup "unfoldr" unfoldr
    , bgroup "unfoldrN" unfoldrN
    ]

singleton :: [Benchmark]
singleton =
    [ bench "bytestring/singleton" $ nf B.singleton 128
    , bench "bytes/singleton"      $ nf (V.singleton @V.PrimVector) (128 :: Word8)
    ]

eq :: [Benchmark]
eq =
    [ bench "bytestring/(==)" $ nf (== bytestring10000) (B.copy bytestring10000)
    , bench "bytes/(==)"      $ nf (== bytes10000) (V.copy bytes10000)
    , bench "bytestring/(==)/same bytestring" $ nf (== bytestring10000) bytestring10000
    , bench "bytes/(==)/same bytes"      $ nf (== bytes10000) bytes10000
    ]

packSmall :: [Benchmark]
packSmall =
    [ bench "bytestring/pack"  $ nf B.pack list100
    , bench "vector/fromList"  $ nf VU.fromList list100
    , bench "bytes/pack"       $ nf (V.pack @V.PrimVector) list100
    , bench "bytes/packN 64"   $ nf (V.packN @V.PrimVector 64)  list100
    , bench "bytes/packN 100"  $ nf (V.packN @V.PrimVector 100) list100
    , bench "bytes/packR"      $ nf (V.packR @V.PrimVector) list100
    ]

packLarge :: [Benchmark]
packLarge =
    [ bench "bytestring/pack"   $ nf B.pack list10000
    , bench "vector/fromList"   $ nf VU.fromList list10000
    , bench "bytes/pack"        $ nf (V.pack @V.PrimVector) list10000
    , bench "bytes/packN 64"    $ nf (V.packN @V.PrimVector 64) list10000
    , bench "bytes/packN 10000" $ nf (V.packN @V.PrimVector 10000) list10000
    , bench "bytes/packR"       $ nf (V.packR @V.PrimVector) list10000
    , bench "bytes/packN 10000/fused" $ nf (\ n -> V.packN @V.PrimVector n (List.replicate n (128 :: Word8))) 10000
    ]

unpack :: [Benchmark]
unpack =
    [ bench "bytestring/unpack"  $ nf B.unpack bytestring1000
    , bench "bytes/unpack"       $ nf V.unpack bytes1000
    ]

map :: [Benchmark]
map =
    [ bench "bytestring/map"  $ nf (\ bs -> B.map (+1) bs) bytestring1000
    , bench "vector/map"      $ nf (VU.map (+1)) vector1000
    , bench "bytes/map"       $ nf (\ bs -> V.map @V.PrimVector @V.PrimVector (+1) bs) bytes1000
    , bench "bytes/map'"       $ nf (\ bs -> V.map' @V.PrimVector @V.PrimVector (+1) bs) bytes1000
    , bench "bytes/pack . List.map f . unpack" $
        nf (V.packN @V.PrimVector 1000 . List.map (+1) . V.unpack) bytes1000
    ]

reverse :: [Benchmark]
reverse =
    [ bench "bytestring/reverse"  $ nf B.reverse bytestring1000
    , bench "vector/reverse"      $ nf VU.reverse vector1000
    , bench "bytes/reverse"       $ nf V.reverse bytes1000
    ]

intersperse :: [Benchmark]
intersperse =
    [ bench "bytestring/intersperse"  $ nf (B.intersperse 0) bytestring1000
    , bench "bytes/intersperse"       $ nf (V.intersperse 0) bytes1000
    ]

intercalate :: [Benchmark]
intercalate =
    [ bench "bytestring/intercalate"  $
        nf (B.intercalate bytestring100) (List.replicate 10 bytestring1000)
    , bench "bytes/intercalate"       $
        nf (V.intercalate bytes100) (List.replicate 10 bytes1000)
    , bench "bytestring/intercalate/rule" $
        nf (B.intercalate (B.singleton 0)) [bytestring1000, bytestring1000]
    , bench "bytestring/intercalateElem"  $
        nf (V.intercalateElem 0) [bytes1000, bytes1000]
    ]

foldl' :: [Benchmark]
foldl' =
    [ bench "bytestring/foldl'" $ nf (\ x -> B.foldl' (+) wordZ x) bytestring1000
    , bench "vector/foldl'"     $ nf (VU.foldl' (+) wordZ) vector1000
    , bench "bytes/foldl'"      $ nf (\ x -> V.foldl' (+) wordZ x) bytes1000
    , bench "bytes/foldl'"      $ nf (List.foldl' (+) wordZ . V.unpack) bytes1000
    ]

foldl :: [Benchmark]
foldl =
    [ bench "bytestring/foldl" $ nf (\ x -> B.foldl (+) wordZ x) bytestring1000
    , bench "vector/foldl"     $ nf (VU.foldl (+) wordZ) vector1000
    , bench "bytes/foldl"      $ nf (List.foldl (+) wordZ . V.unpack) bytes1000
    ]

foldr' :: [Benchmark]
foldr' =
    [ bench "bytestring/foldr'" $ nf (\ x -> B.foldr' (+) wordZ x) bytestring1000
    , bench "vector/foldr'"     $ nf (VU.foldr' (+) wordZ) vector1000
    , bench "bytes/foldr'"      $ nf (\ x -> V.foldr' (+) wordZ x) bytes1000
    ]

foldr :: [Benchmark]
foldr =
    [ bench "bytestring/foldr" $ nf (\ x -> B.foldr (+) wordZ x) bytestring1000
    , bench "vector/foldr"     $ nf (VU.foldr (+) wordZ) vector1000
    , bench "bytes/foldr"      $ nf (List.foldr (+) wordZ . V.unpack) bytes1000
    ]

concat :: [Benchmark]
concat =
    [ bench "bytestring/concat"  $ nf B.concat (List.replicate 1000 bytestring1000)
    , bench "bytes/concat"       $ nf V.concat (List.replicate 1000 bytes1000)
    ]

concatMap :: [Benchmark]
concatMap =
    [ bench "bytestring/concatMap" $ nf (B.concatMap (const bytestring100)) bytestring1000
    , bench "vector/concatMap"     $ nf (VU.concatMap (const vector100)) vector1000
    , bench "bytes/concatMap"      $ nf (V.concatMap (const bytes100)) bytes1000
    ]

all :: [Benchmark]
all =
    [ bench "bytestring/all" $ nf (\ x -> B.all odd x) bytestring1000
    , bench "vector/all"     $ nf (VU.all odd) vector1000
    , bench "bytes/all"      $ nf (\ x -> V.all odd x) bytes1000
    ]

any :: [Benchmark]
any =
    [ bench "bytestring/any" $ nf (\ x -> B.any even x) bytestring1000
    , bench "vector/any"     $ nf (VU.any even) vector1000
    , bench "bytes/any"      $ nf (\x -> V.any even x) bytes1000
    ]

accumStep :: Word8 -> Word8 -> (Word8, Word8)
accumStep x y = (x+1, x*y)

mapAccumL :: [Benchmark]
mapAccumL =
    [ bench "bytestring/mapAccumL" $ nf (\x -> B.mapAccumL accumStep 0 x) bytestring1000
    , bench "bytes/mapAccumL"      $ nf (V.mapAccumL @V.PrimVector @V.PrimVector accumStep 0) bytes1000
    ]

mapAccumR :: [Benchmark]
mapAccumR =
    [ bench "bytestring/mapAccumR" $ nf (\x -> B.mapAccumR accumStep 0 x) bytestring1000
    , bench "bytes/mapAccumR"      $ nf (V.mapAccumR @V.PrimVector @V.PrimVector accumStep 0) bytes1000
    ]

replicate :: [Benchmark]
replicate =
    [ bench "bytestring/replicate" $ nf (B.replicate 1000) (127::Word8)
    , bench "vector/replicate"     $ nf (VU.replicate 1000) (127::Word8)
    , bench "bytes/replicate"      $ nf (V.replicate @V.PrimVector 1000) (127::Word8)
    ]

unfoldrStep :: Word8 -> Maybe (Word8, Word8)
unfoldrStep x | x < 254   = Just (x + 1, x + 1)
              | otherwise = Nothing

unfoldr :: [Benchmark]
unfoldr =
    [ bench "bytestring/unfoldr" $ nf (B.unfoldr unfoldrStep) (0::Word8)
    , bench "vector/unfoldr"     $ nf (VU.unfoldr unfoldrStep) (0::Word8)
    , bench "bytes/unfoldr"      $ nf (V.unfoldr @V.PrimVector unfoldrStep) (0::Word8)
    ]

unfoldrN :: [Benchmark]
unfoldrN =
    [ bench "bytestring/unfoldrN" $ nf (\ z -> B.unfoldrN 200 unfoldrStep z) (0::Word8)
    , bench "vector/unfoldrN"     $ nf (VU.unfoldrN 200 unfoldrStep) (0::Word8)
    , bench "bytes/unfoldrN"      $ nf (V.unfoldrN @V.PrimVector 200 unfoldrStep) (0::Word8)
    ]

