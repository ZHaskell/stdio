{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Serialize.Get
import qualified Data.Binary.Get as G
import qualified Data.Store.Core as C
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Std.Data.Binary hiding (Test(..))
import Std.Data.Parser
import qualified Std.Data.Vector.Base as V
import Control.Monad

data Test = Test
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word16
  deriving Show

getTest :: Get Test
getTest = Test <$> getWord16le
              <*> getWord16le
              <*> getWord16le
              <*> getWord16le
              <*> getWord16le
              <*> getWord16le

getTest' :: G.Get Test
getTest' = Test <$> G.getWord16le
              <*> G.getWord16le
              <*> G.getWord16le
              <*> G.getWord16le
              <*> G.getWord16le
              <*> G.getWord16le

peekTest :: C.Peek Test
peekTest = Test <$> C.peekStorable
                <*> C.peekStorable
                <*> C.peekStorable
                <*> C.peekStorable
                <*> C.peekStorable
                <*> C.peekStorable

decodeTest' :: Parser Test
decodeTest' = Test <$> decodePrimLE
              <*> decodePrimLE
              <*> decodePrimLE
              <*> decodePrimLE
              <*> decodePrimLE
              <*> decodePrimLE

main :: IO ()
main = do
    print "Store"
    forM_ [0..10] $ \ i -> do
        let !b = B.replicate 120000000 (fromIntegral i)
        print $ last $ C.decodeExWith (replicateM 10000000 peekTest) b
    print "Store"
    print "Std.Data.Parser"
    forM_ [0..10] $ \ i -> do
        let !v = V.replicate 120000000 (fromIntegral i)
        print $ last <$> parse (replicateM 10000000 decodeTest') v
    print "Std.Data.Parser"


