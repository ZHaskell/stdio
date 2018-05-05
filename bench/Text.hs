{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Text (text) where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified "text" Data.Text as T
import qualified "stdio" Data.Text as S
import qualified "stdio" Data.Vector as V
import Control.DeepSeq
import Control.Monad
import Control.Exception (evaluate)
import Data.Monoid ((<>))
import Data.Word
import qualified Data.List as List

import Prelude hiding (reverse,head,tail,last,init,null
    ,length,map,lines,foldl,foldr,unlines
    ,concat,any,take,drop,splitAt,takeWhile
    ,dropWhile,span,break,elem,filter,maximum
    ,minimum,all,concatMap,foldl1,foldr1
    ,scanl,scanl1,scanr,scanr1
    ,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn,interact
    ,zip,zipWith,unzip,notElem
    )

text :: T.Text -> S.Text -> [Benchmark]
text t st = List.reverse
    [ bgroup "pack"    (pack1000 t st)
    , bgroup "unpack"  (unpack1000 t st)
    , bgroup "last"    (last t st)
    , bgroup "length"  (length t st)
    , bgroup "map"     (map t st)
    , bgroup "reverse" (reverse t st)
    ]

unpack1000 :: T.Text -> S.Text -> [Benchmark]
unpack1000 t st =
    [ bench "text/unpack" $ nf T.unpack t
    , bench "stdio text/unpack" $ nf S.unpack st
    ]

pack1000 :: T.Text -> S.Text -> [Benchmark]
pack1000 t st =
    [ bench "text/pack" $ nf T.pack (List.replicate 1000 '0')
    , bench "stdio text/pack" $ nf S.pack (List.replicate 1000 '0')
    ]

last :: T.Text -> S.Text -> [Benchmark]
last t st =
    [ bench "text/last" $ nf T.last t
    , bench "stdio text/last" $ nf S.last st
    ]

length :: T.Text -> S.Text -> [Benchmark]
length t st =
    [ bench "text/length" $ nf T.length t
    , bench "stdio text/length" $ nf S.length st
    ]

map :: T.Text -> S.Text -> [Benchmark]
map t st =
    [ bench "text/map" $ nf (T.map id) t
    , bench "stdio text/map" $ nf (S.map id) st
    ]

reverse :: T.Text -> S.Text -> [Benchmark]
reverse t st =
    [ bench "text/reverse" $ nf T.reverse t
    , bench "stdio text/reverse" $ nf S.reverse st
    ]
