{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Criterion.Main
import qualified Data.ByteString as B
import qualified "stdio" Data.Vector as V
import qualified Data.List as List
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Control.DeepSeq
import Builder
import Bytes
import Text
import BitTwiddle
import System.IO (readFile)
import qualified "text" Data.Text as T
import qualified "stdio" Data.Text as S

main :: IO ()
main = do
    str <- readFile "test/utf8-sample.txt"
    let t = T.pack str
        st = S.pack str
    defaultMain -- $ List.reverse  -- uncomment this reverse bench, useful for dev
        [ bgroup "Bytes" bytes
        , bgroup "Builder" builder
        , bgroup "BitTwiddle" bitTwiddle
        , bgroup "Text" (text t st)
        ]

