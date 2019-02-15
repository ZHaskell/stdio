{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : Std.Data.PrimArray.QQ
Description : Extra stuff for PrimArray related literals
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides functions for writing 'PrimArray' related literals 'QuasiQuote'.

-}

module Std.Data.PrimArray.QQ
  ( -- * PrimArray literal quoters
    arrASCII
  , arrW8, arrW16, arrW32, arrW64, arrWord
  , arrI8, arrI16, arrI32, arrI64, arrInt
   -- * quoter helpers
  , asciiLiteral
  , utf8Literal
  , word8Literal
  , word16Literal
  , word32Literal
  , word64Literal
  , wordLiteral
  , int8Literal
  , int16Literal
  , int32Literal
  , int64Literal
  , intLiteral
  , word8ArrayFromAddr
  , word16ArrayFromAddr
  , word32ArrayFromAddr
  , word64ArrayFromAddr
  , wordArrayFromAddr
  , int8ArrayFromAddr
  , int16ArrayFromAddr
  , int32ArrayFromAddr
  , int64ArrayFromAddr
  , intArrayFromAddr
  ) where

#include "MachDeps.h"

import           Control.Monad
import           Data.Bits
import           Data.Char                 (ord)
import           Data.Primitive.PrimArray
import           GHC.Prim
import           GHC.Ptr
import           GHC.Types
import           Data.Word
import           Data.Int
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Std.Data.Array
import           Control.Monad.ST

-- | Construct data with ASCII encoded literals.
--
-- Example usage:
--
-- @
-- arrASCII :: QuasiQuoter
-- arrASCII = QuasiQuoter
--     (asciiLiteral $ \ len addr -> [| word8ArrayFromAddr $(len) $(addr) |])
--     ...
--
-- word8ArrayFromAddr :: Int -> Addr# -> PrimArray Word8
-- {-# INLINE word8ArrayFromAddr #-}
-- word8ArrayFromAddr l addr# = runST $ do
--     mba <- newPrimArray (I# l)
--     copyPtrToMutablePrimArray mba 0 (Ptr addr#) l
--     unsafeFreezePrimArray mba
-- @
--
asciiLiteral :: (ExpQ -> ExpQ -> ExpQ) -- ^ Construction function which receive a byte
                                       --   length 'Int' and a 'Addr#' 'LitE' expression.
             -> String                 -- ^ Quoter input
             -> ExpQ                   -- ^ Final Quoter
asciiLiteral k str = k (return . LitE  . IntegerL . fromIntegral $ length str)
                       ((LitE . StringPrimL) `fmap` check str)
  where
    check :: String -> Q [Word8]
    check [] = return []
    check (c:cs) = do
        when (ord c > 0xFF) $
            fail $ "character '" ++ [c] ++ "' is have out of range in ASCII literal:" ++ str
        cs' <- check cs
        return (fromIntegral (ord c):cs')

arrASCII :: QuasiQuoter
arrASCII = QuasiQuoter
    (asciiLiteral $ \ len addr -> [| word8ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrASCII as a pattern")
    (error "Cannot use arrASCII as a type")
    (error "Cannot use arrASCII as a dec")

word8ArrayFromAddr :: Int -> Addr# -> PrimArray Word8
{-# INLINE word8ArrayFromAddr #-}
word8ArrayFromAddr l addr# = runST $ do
    mba <- newPrimArray l
    copyPtrToMutablePrimArray mba 0 (Ptr addr#) l
    unsafeFreezePrimArray mba

int8ArrayFromAddr :: Int -> Addr# -> PrimArray Int8
int8ArrayFromAddr l addr# = castArray (word8ArrayFromAddr l addr#)

-- | Construct data with UTF-8 encoded literals.
--
-- Smiliar to 'asciIILiteral', the
--
utf8Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
utf8Literal k str = k (return . LitE  . IntegerL . fromIntegral $ length str)
                      ((LitE . StringPrimL) `fmap` check str)
  where
    check :: String -> Q [Word8]
    check [] = return []
    check (c:cs) = case ord c of
        n
            | n <= 0x0000007F -> do
                let w = fromIntegral n
                ws <- check cs
                return (w:ws)
            | n <= 0x000007FF -> do
                let w1 = fromIntegral $ 0xC0 .|. (n `shiftR` 6)
                    w2 = fromIntegral $ 0x80 .|. (n .&. 0x3F)
                ws <- check cs
                return (w1:w2:ws)
            | n <= 0x0000D7FF -> do
                let w1 = fromIntegral $ 0xE0 .|. (n `shiftR` 12)
                    w2 = fromIntegral $ 0x80 .|. (n `shiftR` 6 .&. 0x3F)
                    w3 = fromIntegral $ 0x80 .|. (n .&. 0x3F)
                ws <- check cs
                return (w1:w2:w3:ws)
            | n <= 0x0000DFFF -> do
                fail $ "character '" ++ [c] ++ "' is have out of range in UTF-8 literal:" ++ str
            | n <= 0x0000FFFF -> do
                let w1 = fromIntegral $ 0xE0 .|. (n `shiftR` 12)
                    w2 = fromIntegral $ 0x80 .|. (n `shiftR` 6 .&. 0x3F)
                    w3 = fromIntegral $ 0x80 .|. (n .&. 0x3F)
                ws <- check cs
                return (w1:w2:w3:ws)
            | n <= 0x0010FFFF -> do
                let w1 = fromIntegral $ 0xF0 .|. (n `shiftR` 18)
                    w2 = fromIntegral $ 0x80 .|. (n `shiftR` 12 .&. 0x3F)
                    w3 = fromIntegral $ 0x80 .|. (n `shiftR` 6 .&. 0x3F)
                    w4 = fromIntegral $ 0x80 .|. (n .&. 0x3F)
                ws <- check cs
                return (w1:w2:w3:w4:ws)
            | otherwise ->
                fail $ "character '" ++ [c] ++ "' is have out of range in UTF-8 literal:" ++ str


vectorLiteral :: ([Integer] -> Q [Word8]) -> (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
vectorLiteral f k str = do
    (len, ws) <- parse str
    k (return . LitE  . IntegerL .fromIntegral $ len) $ (return . LitE . StringPrimL) ws
  where
    parse :: String -> Q (Int, [Word8])
    parse str = do
        case (readList :: ReadS [Integer]) ("[" ++ str ++ "]") of
            [(is, "")] -> (length is, ) `fmap` f is
            _ -> do _ <- fail $ "can't parse vector literal:" ++ str
                    return (0, [])

--------------------------------------------------------------------------------

word8Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
word8Literal k str = vectorLiteral checkW8 k str
  where
    checkW8 :: [Integer] -> Q [Word8]
    checkW8 [] = return []
    checkW8 (i:is) = do
        when (i<0 || i > 0xFF) $
            fail $ "integer " ++ show i ++ " is out of Word8 range in literal:" ++ str
        ws <- checkW8 is
        let w = fromIntegral (i .&. 0xFF)
        return (w:ws)

arrW8 :: QuasiQuoter
arrW8 = QuasiQuoter
    (word8Literal $ \ len addr -> [| word8ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrW8 as a pattern")
    (error "Cannot use arrW8 as a type")
    (error "Cannot use arrW8 as a dec")

int8Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
int8Literal k str = vectorLiteral checkI8 k str
  where
    checkI8 :: [Integer] -> Q [Word8]
    checkI8 [] = return []
    checkI8 (i:is) = do
        when (i< (-0x80) || i > 0x7F) $
            fail $ "integer " ++ show i ++ " is out of Int8 range in literal:" ++ str
        ws <- checkI8 is
        let w = fromIntegral (i .&. 0xFF)
        return (w:ws)

arrI8 :: QuasiQuoter
arrI8 = QuasiQuoter
    (int8Literal $ \ len addr -> [| int8ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrI8 as a pattern")
    (error "Cannot use arrI8 as a type")
    (error "Cannot use arrI8 as a dec")

--------------------------------------------------------------------------------

word16Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
word16Literal k str = vectorLiteral checkW16 k str
  where
    checkW16 :: [Integer] -> Q [Word8]
    checkW16 [] = return []
    checkW16 (i:is) = do
        when (i<0 || i > 0xFFFF) $
            fail $ "integer " ++ show i ++ " is out of word16 range in literal:" ++ str
        ws <- checkW16 is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
#ifdef WORDS_BIGENDIAN
        return (w2:w1:ws)
#else
        return (w1:w2:ws)
#endif

arrW16 :: QuasiQuoter
arrW16 = QuasiQuoter
    (word16Literal $ \ len addr -> [| word16ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrW16 as a pattern")
    (error "Cannot use arrW16 as a type")
    (error "Cannot use arrW16 as a dec")

word16ArrayFromAddr :: Int -> Addr# -> PrimArray Word16
{-# INLINE word16ArrayFromAddr #-}
word16ArrayFromAddr l addr# = runST $ do
    mba <- newArr l
    copyPtrToMutablePrimArray mba 0 (Ptr addr#) l
    unsafeFreezePrimArray mba

int16ArrayFromAddr :: Int -> Addr# -> PrimArray Int16
int16ArrayFromAddr l addr# = castArray (word16ArrayFromAddr l addr#)

int16Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
int16Literal k str = vectorLiteral checkI16 k str
  where
    checkI16 :: [Integer] -> Q [Word8]
    checkI16 [] = return []
    checkI16 (i:is) = do
        when (i<(-0x8000) || i>0x7FFF) $
            fail $ "integer " ++ show i ++ " is out of int16 range in literal:" ++ str
        ws <- checkI16 is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
#ifdef WORDS_BIGENDIAN
        return (w2:w1:ws)
#else
        return (w1:w2:ws)
#endif

arrI16 :: QuasiQuoter
arrI16 = QuasiQuoter
    (word16Literal $ \ len addr -> [| int16ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrI16 as a pattern")
    (error "Cannot use arrI16 as a type")
    (error "Cannot use arrI16 as a dec")
--------------------------------------------------------------------------------

word32Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
word32Literal k str = vectorLiteral checkW32 k str
  where
    checkW32 :: [Integer] -> Q [Word8]
    checkW32 [] = return []
    checkW32 (i:is) = do
        when (i<0 || i > 0xFFFFFFFF) $
            fail $ "integer " ++ show i ++ " is out of word32 range in literal:" ++ str
        ws <- checkW32 is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
            w3 = fromIntegral (i `shiftR` 16 .&. 0xFF)
            w4 = fromIntegral (i `shiftR` 24 .&. 0xFF)
#ifdef WORDS_BIGENDIAN
        return (w4:w3:w2:w1:ws)
#else
        return (w1:w2:w3:w4:ws)
#endif

arrW32 :: QuasiQuoter
arrW32 = QuasiQuoter
    (word32Literal $ \ len addr -> [| word32ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrW32 as a pattern")
    (error "Cannot use arrW32 as a type")
    (error "Cannot use arrW32 as a dec")

word32ArrayFromAddr :: Int -> Addr# -> PrimArray Word32
{-# INLINE word32ArrayFromAddr #-}
word32ArrayFromAddr l addr# = runST $ do
    mba <- newArr l
    copyPtrToMutablePrimArray mba 0 (Ptr addr#) l
    unsafeFreezePrimArray mba

int32ArrayFromAddr :: Int -> Addr# -> PrimArray Int32
int32ArrayFromAddr l addr# = castArray (word32ArrayFromAddr l addr#)

int32Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
int32Literal k str = vectorLiteral checkI32 k str
  where
    checkI32 :: [Integer] -> Q [Word8]
    checkI32 [] = return []
    checkI32 (i:is) = do
        when (i<(-0x80000000) || i>0x7FFFFFFF) $
            fail $ "integer " ++ show i ++ " is out of int32 range in literal:" ++ str
        ws <- checkI32 is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
            w3 = fromIntegral (i `shiftR` 16 .&. 0xFF)
            w4 = fromIntegral (i `shiftR` 24 .&. 0xFF)
#ifdef WORDS_BIGENDIAN
        return (w4:w3:w2:w1:ws)
#else
        return (w1:w2:w3:w4:ws)
#endif

arrI32 :: QuasiQuoter
arrI32 = QuasiQuoter
    (int32Literal $ \ len addr -> [| int32ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrI32 as a pattern")
    (error "Cannot use arrI32 as a type")
    (error "Cannot use arrI32 as a dec")

--------------------------------------------------------------------------------

word64Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
word64Literal k str = vectorLiteral checkW64 k str
  where
    checkW64 :: [Integer] -> Q [Word8]
    checkW64 [] = return []
    checkW64 (i:is) = do
        when (i<0 || i > 0xFFFFFFFFFFFFFFFF) $
            fail $ "integer " ++ show i ++ " is out of word64 range in literal:" ++ str
        ws <- checkW64 is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
            w3 = fromIntegral (i `shiftR` 16 .&. 0xFF)
            w4 = fromIntegral (i `shiftR` 24 .&. 0xFF)
            w5 = fromIntegral (i `shiftR` 32 .&. 0xFF)
            w6 = fromIntegral (i `shiftR` 40 .&. 0xFF)
            w7 = fromIntegral (i `shiftR` 48 .&. 0xFF)
            w8 = fromIntegral (i `shiftR` 56 .&. 0xFF)
#ifdef WORDS_BIGENDIAN
        return (w8:w7:w6:w5:w4:w3:w2:w1:ws)
#else
        return (w1:w2:w3:w4:w5:w6:w7:w8:ws)
#endif

arrW64 :: QuasiQuoter
arrW64 = QuasiQuoter
    (word64Literal $ \ len addr -> [| word64ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrW64 as a pattern")
    (error "Cannot use arrW64 as a type")
    (error "Cannot use arrW64 as a dec")

word64ArrayFromAddr :: Int -> Addr# -> PrimArray Word64
{-# INLINE word64ArrayFromAddr #-}
word64ArrayFromAddr l addr# = runST $ do
    mba <- newArr l
    copyPtrToMutablePrimArray mba 0 (Ptr addr#) l
    unsafeFreezePrimArray mba

int64ArrayFromAddr :: Int -> Addr# -> PrimArray Int64
int64ArrayFromAddr l addr# = castArray (word64ArrayFromAddr l addr#)

int64Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
int64Literal k str = vectorLiteral checkI64 k str
  where
    checkI64 :: [Integer] -> Q [Word8]
    checkI64 [] = return []
    checkI64 (i:is) = do
        when (i<(-0x8000000000000000) || i > 0x7FFFFFFFFFFFFFFF) $
            fail $ "integer " ++ show i ++ " is out of int64 range in literal:" ++ str
        ws <- checkI64 is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
            w3 = fromIntegral (i `shiftR` 16 .&. 0xFF)
            w4 = fromIntegral (i `shiftR` 24 .&. 0xFF)
            w5 = fromIntegral (i `shiftR` 32 .&. 0xFF)
            w6 = fromIntegral (i `shiftR` 40 .&. 0xFF)
            w7 = fromIntegral (i `shiftR` 48 .&. 0xFF)
            w8 = fromIntegral (i `shiftR` 56 .&. 0xFF)
#ifdef WORDS_BIGENDIAN
        return (w8:w7:w6:w5:w4:w3:w2:w1:ws)
#else
        return (w1:w2:w3:w4:w5:w6:w7:w8:ws)
#endif

arrI64 :: QuasiQuoter
arrI64 = QuasiQuoter
    (int64Literal $ \ len addr -> [| int64ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrI64 as a pattern")
    (error "Cannot use arrI64 as a type")
    (error "Cannot use arrI64 as a dec")

--------------------------------------------------------------------------------

wordArrayFromAddr :: Int -> Addr# -> PrimArray Word
wordArrayFromAddr l addr# =
#if SIZEOF_HSWORD == 8
    unsafeCoerce# (word64ArrayFromAddr l addr#)
#else
    unsafeCoerce# (word32ArrayFromAddr l addr#)
#endif

intArrayFromAddr :: Int -> Addr# -> PrimArray Int
intArrayFromAddr l addr# =
#if SIZEOF_HSWORD == 8
    unsafeCoerce# (int64ArrayFromAddr l addr#)
#else
    unsafeCoerce# (int32ArrayFromAddr l addr#)
#endif

wordLiteral :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
wordLiteral =
#if SIZEOF_HSWORD == 8
    word64Literal
#else
    word32Literal
#endif

intLiteral :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
intLiteral =
#if SIZEOF_HSWORD == 8
    int64Literal
#else
    int32Literal
#endif

arrWord :: QuasiQuoter
arrWord = QuasiQuoter
    (wordLiteral $ \ len addr -> [| wordArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrWord as a pattern")
    (error "Cannot use arrWord as a type")
    (error "Cannot use arrWord as a dec")

arrInt :: QuasiQuoter
arrInt = QuasiQuoter
    (intLiteral $ \ len addr -> [| intArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrInt as a pattern")
    (error "Cannot use arrInt as a type")
    (error "Cannot use arrInt as a dec")

--------------------------------------------------------------------------------
