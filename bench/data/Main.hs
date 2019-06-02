{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Control.DeepSeq
import           Criterion.Main
import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Vector.Unboxed as VU
import           Data.Word
import           System.IO (readFile)
import qualified Data.Text as T
import qualified Std.Data.Text as S
import qualified Std.Data.Vector as V

import Builder
import Bytes
import Text
import BitTwiddle

main :: IO ()
main = do
    str <- readFile "./utf8-sample.txt"
    let t = T.pack str
        st = S.pack str
    defaultMain -- $ List.reverse  -- uncomment this reverse bench, useful for dev
        [ bgroup "Bytes" bytes
        , bgroup "Builder" builder
        , bgroup "BitTwiddle" bitTwiddle
        , bgroup "Text" (text t st)
        ]

