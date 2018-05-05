{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MagicHash #-}

module BitTwiddle (bitTwiddle) where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified "stdio" Data.Vector as V
import Control.DeepSeq
import Control.Monad
import Data.Word
import GHC.Prim
import GHC.Types
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import qualified Data.Primitive.BitTwiddle as T
import qualified Data.List as List

bytestring1000000 :: BS.ByteString
bytestring1000000 = BS.replicate 1000000 0

bytes1000000 :: V.Bytes
bytes1000000 = V.replicate 1000000 0


bitTwiddle :: [Benchmark]
bitTwiddle =
    [ bgroup "memchr 1000000" memchr
    , bgroup "memcnt 1000000" memchrReverse
    ]

memchr :: [Benchmark]
memchr =
    [ bench "bytestring/elemIndex" $ nf (BS.elemIndex 1) bytestring1000000
    , bench "bit-twiddling/memchr" $ nf (\ (V.PrimVector ba s l) -> T.memchr ba 1 s l) bytes1000000
    ]

memchrReverse :: [Benchmark]
memchrReverse =
    [ bench "bytestring/elemIndexReverse" $ nf (BS.elemIndexEnd 1) bytestring1000000
    , bench "bit-twiddling/memchrReverse" $ nf (\ (V.PrimVector ba s l) -> T.memchrReverse ba 1 s l) bytes1000000
    ]
