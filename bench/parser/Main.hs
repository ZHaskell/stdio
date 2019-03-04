{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Serialize.Get
import qualified Data.Binary.Get as G
import qualified  Data.Attoparsec.ByteString as AP
import qualified  Data.Attoparsec.Internal as AP
import qualified Data.Store.Core as C
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Std.Data.Parser
import qualified Std.Data.Vector.Base as V
import Control.Monad
import qualified Foreign.Ptr as AP (castPtr, minusPtr, plusPtr)
import qualified Foreign.ForeignPtr as AP (withForeignPtr)
import qualified Foreign.Storable as AP (Storable(peek, sizeOf))
import qualified Data.ByteString.Internal as B

storable :: AP.Storable a => AP.Parser a
{-# INLINE storable #-}
storable = hack undefined
 where
  hack :: AP.Storable b => b -> AP.Parser b
  hack dummy = do
    (fp,o,_) <- B.toForeignPtr `fmap` AP.take (AP.sizeOf dummy)
    return . B.inlinePerformIO . AP.withForeignPtr fp $ \p ->
        AP.peek (AP.castPtr $ p `AP.plusPtr` o)

data Test = Test
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word16
  deriving Show

apTest :: AP.Parser Test
apTest = Test <$> storable
              <*> storable
              <*> storable
              <*> storable
              <*> storable
              <*> storable

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
    print "Store start"
    forM_ [0..10] $ \ i -> do
        let !b = B.replicate 120000000 (fromIntegral i)
        print $ last $ C.decodeExWith (replicateM 10000000 peekTest) b
    print "Store end"
    print "Std.Data.Parser start"
    forM_ [0..10] $ \ i -> do
        let !v = V.replicate 120000000 (fromIntegral i)
        print $ last <$> parse_ (replicateM 10000000 decodeTest') v
    print "Std.Data.Parser end"
    print "attoparsec start"
    forM_ [0..10] $ \ i -> do
        let !b = B.replicate 120000000 (fromIntegral i)
        print $ last <$> AP.parseOnly (replicateM 10000000 apTest) b
    print "attoparsec end"
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
