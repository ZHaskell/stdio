{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Data.Aeson
import qualified Std.Data.JSON.Value as JSON
import qualified Std.Data.JSON.Base as JSON
import qualified Std.Data.Builder as B

import qualified Auto.T.D as T
import qualified Auto.T.BigRecord as T
import qualified Auto.T.BigProduct as T
import qualified Auto.T.BigSum as T
import qualified Auto.G.D as G
import qualified Auto.G.BigRecord as G
import qualified Auto.G.BigProduct as G
import qualified Auto.G.BigSum as G

--------------------------------------------------------------------------------

runBench :: IO ()
runBench = defaultMain
  [ compareBench "D" T.d G.d
  , compareBench "BigRecord" T.bigRecord G.bigRecord
  , compareBench "BigProduct" T.bigProduct G.bigProduct
  , compareBench "BigSum" T.bigSum G.bigSum
  ]

group :: String -> Benchmarkable -> Benchmarkable -> Benchmark
group n th gen = bgroup n [ bench "th"      th
                          , bench "generic" gen
                          ]

compareBench
  :: forall a b
  .  (ToJSON a, FromJSON a, NFData a, ToJSON b, FromJSON b, NFData b
     , JSON.ToJSON b, JSON.FromJSON b , JSON.EncodeJSON b)

  => String -> a -> b -> Benchmark
compareBench name a b = v `deepseq` bgroup name
  [ group "toJSON"   (nf toJSON a)
                     (nf toJSON b)
  , bench "stdio-toJSON" (nf JSON.toJSON b)
  , group "encode"   (nf encode a)
                     (nf encode b)
  , bench "stdio-encode"   (nf encode' b)
  , group "fromJSON" (nf (fromJSON :: Value -> Result a) v)
                     (nf (fromJSON :: Value -> Result b) v)
  , bench "stdio-fromJSON" (nf (decode' :: JSON.Value -> JSON.Result b) v')
  ] where
    v = toJSON a  -- == toJSON b
    v' = JSON.toJSON b
    encode' = B.buildBytes . JSON.encodeJSON
    decode' = JSON.parseEither (JSON.fromJSON)

sanityCheck :: IO ()
sanityCheck = do
  check T.d
  check G.d
  check T.bigRecord
  check G.bigRecord
  check T.bigProduct
  check G.bigProduct
  check T.bigSum
  check G.bigSum

  check' G.d
  check' G.bigRecord
  check' G.bigProduct
  check' G.bigSum

check :: (Show a, Eq a, FromJSON a, ToJSON a)
      => a -> IO ()
check x = do
  unless (Success x == (fromJSON . toJSON) x) $ fail $ "toJSON: " ++ show x
  unless (Success x == (decode_ . encode) x) $ fail $ "encode: " ++ show x
  where
    decode_ s = case decode s of
      Just v -> fromJSON v
      Nothing -> fail ""

check' :: (Show a, Eq a, JSON.FromJSON a, JSON.ToJSON a, JSON.EncodeJSON a)
      => a -> IO ()
check' x = do
    unless (Right x == (JSON.fromJSON . JSON.toJSON) x) $ fail $ "toJSON: " ++ show x
    unless (Right x == (decode' . encode') x) $ fail $ "encode: " ++ show x
  where
    encode' = B.buildBytes . JSON.encodeJSON
    decode' = JSON.parseEither (JSON.fromJSON)

main :: IO ()
main = do
  sanityCheck
  runBench
