{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Builder (builder) where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Builder as B
import qualified "stdio" Data.Binary as B
import qualified "stdio" Data.Vector as V
import Control.DeepSeq
import Control.Monad
import Control.Exception (evaluate)
import Data.Monoid ((<>))
import Data.Word

bytestring1000 :: BS.ByteString
bytestring1000 = BS.replicate 1000 0

bytes1000 :: V.Bytes
bytes1000 = V.replicate 1000 0

bytestring20000 :: BS.ByteString
bytestring20000 = BS.replicate 20000 0

bytes20000 :: V.Bytes
bytes20000 = V.replicate 20000 0

builder :: [Benchmark]
builder =
    [ bgroup "word8 100000000" word8_100000000
    , bgroup "word8 10000" word8_10000
    , bgroup "word8 32" word8_32
    , bgroup "bytestring/bytes 32 * 1000" bytes_32_1000
    , bgroup "bytestring/bytes 32 * 20000" bytes_32_20000
    ]

word8_100000000 :: [Benchmark]
word8_100000000 =
    [ bench "bytestring/toLazyByteString" $ nf BB.toLazyByteString (mconcat (replicate 100000000 (BB.word8 123)))
    , bench "bytestring/toStrict . toLazyByteString" $ nf (BL.toStrict . BB.toLazyByteString) (mconcat (replicate 100000000 (BB.word8 123)))
    , bench "stdio/buildBytesList"     $ nf B.buildBytesList (mconcat (replicate 100000000 (B.binary @Word8 123)))
    , bench "stdio/buildBytes"     $ nf B.buildBytes (mconcat (replicate 100000000 (B.binary @Word8 123)))
    , bench "stdio/buildAndRun"     $ nfIO (B.buildAndRun (void . evaluate) (mconcat (replicate 100000000 (B.binary @Word8 123))))
    ]

word8_10000 :: [Benchmark]
word8_10000 =
    [ bench "bytestring/toLazyByteString" $ nf BB.toLazyByteString (mconcat (replicate 10000 (BB.word8 123)))
    , bench "bytestring/toStrict . toLazyByteString" $ nf (BL.toStrict . BB.toLazyByteString) (mconcat (replicate 10000 (BB.word8 123)))
    , bench "stdio/buildBytesList"     $ nf B.buildBytesList (mconcat (replicate 10000 (B.binary @Word8 123)))
    , bench "stdio/buildBytes"     $ nf B.buildBytes (mconcat (replicate 10000 (B.binary @Word8 123)))
    , bench "stdio/buildAndRun"     $ nfIO (B.buildAndRun (void . evaluate) (mconcat (replicate 10000 (B.binary @Word8 123))))
    ]

word8_32 :: [Benchmark]
word8_32 =
    [ bench "bytestring/toLazyByteString" $ nf BB.toLazyByteString (mconcat (replicate 32 (BB.word8 123)))
    , bench "bytestring/toStrict . toLazyByteString" $ nf (BL.toStrict . BB.toLazyByteString) (mconcat (replicate 32 (BB.word8 123)))
    , bench "stdio/buildBytesList"     $ nf B.buildBytesList (mconcat (replicate 32 (B.binary @Word8 123)))
    , bench "stdio/buildBytes"     $ nf B.buildBytes (mconcat (replicate 32 (B.binary @Word8 123)))
    , bench "stdio/buildAndRun"     $ nfIO (B.buildAndRun (void . evaluate) (mconcat (replicate 32 (B.binary @Word8 123))))
    ]

bytes_32_1000 :: [Benchmark]
bytes_32_1000 =
    [ bench "bytestring/toLazyByteString" $ nf BB.toLazyByteString
        (mconcat (replicate 32 $ BB.byteString bytestring1000))
    , bench "stdio/buildBytesList"     $ nf B.buildBytesList (mconcat (replicate 32 (B.binary bytes1000)))
    , bench "stdio/buildAndRun"     $ nfIO (B.buildAndRun (void . evaluate) (mconcat (replicate 32 (B.binary bytes1000))))
    ]

bytes_32_20000 :: [Benchmark]
bytes_32_20000 =
    [ bench "bytestring/toLazyByteString" $ nf BB.toLazyByteString
        (mconcat (replicate 32 $ BB.byteString bytestring20000))
    , bench "stdio/buildBytesList"     $ nf B.buildBytesList (mconcat (replicate 32 (B.binary bytes20000)))
    , bench "stdio/buildAndRun"     $ nfIO (B.buildAndRun (void . evaluate) (mconcat (replicate 32 (B.binary bytes20000))))
    ]

