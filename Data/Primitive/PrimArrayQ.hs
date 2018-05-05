{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Data.Primitive.PrimArrayQ where

#include "MachDeps.h"

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import GHC.Word
import GHC.Types
import GHC.Prim
import Control.Monad
import Data.Char (ord)
import Data.Bits
import System.IO.Unsafe
import Data.Array
import Data.Primitive.PrimArray
import GHC.Ptr

asciiLiteral :: (Int -> ExpQ -> ExpQ) -> String -> ExpQ
asciiLiteral k str = k (length str) $ (LitE . StringPrimL) `fmap` check str
  where
    check :: String -> Q [Word8]
    check [] = return []
    check (c:cs) = do
        when (ord c > 0xFF) $
            reportError $ "character '" ++ [c] ++ "' is have out of range in ASCII literal:" ++ str
        cs' <- check cs
        return (fromIntegral (ord c):cs')

asciiLiteralMulLine :: (Int -> ExpQ -> ExpQ) -> String -> ExpQ
asciiLiteralMulLine k str = k (length str) $ (LitE . StringPrimL) `fmap` check str
  where
    check :: String -> Q [Word8]
    check [] = return []
    check (c:cs) = do
        when (ord c > 0xFF) $
            reportWarning $ "character '" ++ [c] ++ "' is out of ASCII range in literal:" ++ str
        cs' <- check cs
        return (fromIntegral (ord c):cs')


utf8Literal :: (Int -> ExpQ -> ExpQ) -> String -> ExpQ
utf8Literal = undefined

--------------------------------------------------------------------------------

vectorLiteral :: ([Integer] -> Q [Word8]) -> (Int -> ExpQ -> ExpQ) -> String -> ExpQ
vectorLiteral f k str = do
    (len, ws) <- parse str
    k len $ (return . LitE . StringPrimL) ws
  where
    parse :: String -> Q (Int, [Word8])
    parse str = do
        case (readList :: ReadS [Integer]) ("[" ++ str ++ "]") of
            [(is, "")] -> (length is, ) `fmap` f is
            _ -> do reportError $ "can't parse vector literal:" ++ str
                    return (0, [])

word16LiteralLE :: (Int -> ExpQ -> ExpQ) -> String -> ExpQ
word16LiteralLE k str = vectorLiteral mkWord16LE k str
  where
    mkWord16LE :: [Integer] -> Q [Word8]
    mkWord16LE [] = return []
    mkWord16LE (i:is) = do
        when (i<0 || i > 0xFFFF) $
            reportError $ "integer " ++ show i ++ " is out of word16 range in literal:" ++ str
        ws <- mkWord16LE is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
        return (fromIntegral w1:w2:ws)

aW16 :: QuasiQuoter
aW16 = QuasiQuoter
    (word16LiteralLE $ \ len addr -> [| word16ArrayFromAddr len $(addr) |])
    (error "Cannot use aW16 as a pattern")
    (error "Cannot use aW16 as a type")
    (error "Cannot use aW16 as a dec")

word16ArrayFromAddr :: Int -> Addr# -> PrimArray Word16
word16ArrayFromAddr l addr# = unsafeDupablePerformIO $ do
    mba <- newArr l
    go l (Ptr addr#) mba 0
    unsafeFreezePrimArray mba :: IO (PrimArray Word16)
  where
    go l ptr mba idx = do
#ifdef WORDS_BIGENDIAN
        when (idx < l) $ do
            w1 <- peekElemOff ptr (idx*2) :: IO Word8
            w2 <- peekElemOff ptr (idx*2+1) :: IO Word8
            writePrimArray mba idx (fromIntegral w2 `shiftL` 8 .|. fromIntegral w1 :: Word16)
            go l ptr mba (idx+1)
#else
        copyMutablePrimArrayFromPtr mba 0 ptr l
#endif
{-# NOINLINE word16ArrayFromAddr #-} -- don't dump every literal with this code

word32LiteralLE :: (Int -> ExpQ -> ExpQ) -> String -> ExpQ
word32LiteralLE k str = vectorLiteral mkWord32LE k str
  where
    mkWord32LE :: [Integer] -> Q [Word8]
    mkWord32LE [] = return []
    mkWord32LE (i:is) = do
        when (i<0 || i > 0xFFFFFFFF) $
            reportError $ "integer " ++ show i ++ " is out of word32 range in literal:" ++ str
        ws <- mkWord32LE is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
            w3 = fromIntegral (i `shiftR` 16 .&. 0xFF)
            w4 = fromIntegral (i `shiftR` 24 .&. 0xFF)
        return (fromIntegral w1:w2:w3:w4:ws)

aW32 :: QuasiQuoter
aW32 = QuasiQuoter
    (word32LiteralLE $ \ len addr -> [| word32ArrayFromAddr len $(addr) |])
    (error "Cannot use aW32 as a pattern")
    (error "Cannot use aW32 as a type")
    (error "Cannot use aW32 as a dec")

word32ArrayFromAddr :: Int -> Addr# -> PrimArray Word32
word32ArrayFromAddr l addr# = unsafeDupablePerformIO $ do
    mba <- newArr l
    go l (Ptr addr#) mba 0
    unsafeFreezePrimArray mba :: IO (PrimArray Word32)
  where
    go l ptr mba !idx = do
#ifdef WORDS_BIGENDIAN
        when (idx < l) $ do
            w1 <- peekElemOff ptr (idx*4) :: IO Word8
            w2 <- peekElemOff ptr (idx*4+1) :: IO Word8
            w3 <- peekElemOff ptr (idx*4+2) :: IO Word8
            w4 <- peekElemOff ptr (idx*4+3) :: IO Word8
            writePrimArray mba idx (fromIntegral w4 `shiftL` 24
                                .|. fromIntegral w3 `shiftL` 16
                                .|. fromIntegral w2 `shiftL` 8
                                .|. fromIntegral w1 :: Word32)
            go l ptr mba (idx+1)
#else
        copyMutablePrimArrayFromPtr mba 0 ptr l  -- alright, we don't support mix endianess
#endif
{-# NOINLINE word32ArrayFromAddr #-}
