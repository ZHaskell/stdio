{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Serialize.Get
import qualified Data.Binary.Get as G
import qualified Data.Store.Core as C
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
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
    print "Cereal start"
    forM_ [0..10] $ \ i -> do
        let !b = B.replicate 120000000 (fromIntegral i)
        print $ last <$> runGet (replicateM 10000000 getTest) b
    print "Cereal end"
    print "Binary start"
    forM_ [0..10] $ \ i -> do
        let !b = B.replicate 120000000 (fromIntegral i)
        print $ last $ G.runGet (replicateM 10000000 getTest') (BL.fromStrict b)
    print "Binary end"
    print "Store start"
    forM_ [0..10] $ \ i -> do
        let !b = B.replicate 120000000 (fromIntegral i)
        print $ last $ C.decodeExWith (replicateM 10000000 peekTest) b
    print "Store end"
    print "Std.Data.Parser start"
    forM_ [0..10] $ \ i -> do
        let !v = V.replicate 120000000 (fromIntegral i)
        print $ last <$> parse (replicateM 10000000 decodeTest') v
    print "Std.Data.Parser end"


