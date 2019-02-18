{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

#if __GLASGOW_HASKELL__ >= 800
#define HAS_DATA_KIND
#endif

{-|
Module      : Std.Data.LEON
Description : Simple binary serialization/deserialization
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

LEON (/L/ittle /E/ndian first /O/bject /N/otation) is an efficiently serialization using simple binary encoding. As suggested by its name, default instances use little endian encoding, i.e. "Intel convention". We do provide instances for 'BE' type which explicitly write wrapped value in big endian though. encoded data should be portable across machine endianness, word size, or compiler version within one major version. For example, data encoded using the 'LEON' class could be written on any machine, and read back on any another using @stdio@ packages with the same major version.

-}

module Std.Data.LEON
  ( LEON(..)
  , BE(..)
  ) where

import           Control.Monad
import           Data.Bits
import           Data.Functor.Identity              (Identity (..))
import qualified Data.List                          as List
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Monoid                        as Monoid
import           Data.Primitive
import           Data.Primitive.PrimArray
import qualified Data.Semigroup                     as Semigroup
import           GHC.Generics
import           GHC.Fingerprint
import           GHC.Int
import           GHC.Natural
import           GHC.Types
import           GHC.Word
import           Data.Version (Version(..))
import           Std.Data.Builder                   as B
import qualified Std.Data.CBytes                    as CBytes
import           Std.Data.Parser                    as P
import           Std.Data.PrimArray.UnalignedAccess
import qualified Std.Data.Text.Base                 as T
import qualified Std.Data.Vector.Base               as V

#include "MachDeps.h"

-- Factored into two classes because this makes GHC optimize the
-- instances faster.  This doesn't matter for builds of stdio,
-- but it matters a lot for end-users who write 'instance LEON T'.
-- See also: https://ghc.haskell.org/trac/ghc/ticket/9630
class GLEONEncode f where
    gencode :: f t -> Builder ()

class GLEONDecode f where
    gdecode :: Parser (f t)

-- | LEON, \L\ittle Endian Object Notation.
class LEON a where
    encode :: a -> Builder ()
    decode :: Parser a

    default encode :: (Generic a, GLEONEncode (Rep a)) => a -> Builder ()
    encode = gencode . from

    default decode :: (Generic a, GLEONDecode (Rep a)) => Parser a
    decode = to `fmap` gdecode

instance LEON Word8 where
    {-# INLINE encode #-}
    encode = encodePrim
    {-# INLINE decode #-}
    decode = decodePrim

instance LEON Word where
    {-# INLINE encode #-}
    encode x = encodePrimLE (fromIntegral x :: Word64)
    {-# INLINE decode #-}
    decode = fromIntegral <$> (decodePrimLE :: Parser Word64)

instance LEON (BE Word) where
    {-# INLINE encode #-}
    encode (BE x) = encodePrimBE (fromIntegral x :: Word64)
    {-# INLINE decode #-}
    decode = BE . fromIntegral <$> (decodePrimBE :: Parser Word64)

instance LEON Int8 where
    {-# INLINE encode #-}
    encode = encodePrim
    {-# INLINE decode #-}
    decode = decodePrim

instance LEON Int where
    {-# INLINE encode #-}
    encode x = encodePrimLE (fromIntegral x :: Int64)
    {-# INLINE decode #-}
    decode = fromIntegral <$> (decodePrimLE :: Parser Int64)

instance LEON (BE Int) where
    {-# INLINE encode #-}
    encode (BE x) = encodePrimBE (fromIntegral x :: Int64)
    {-# INLINE decode #-}
    decode = BE . fromIntegral <$> (decodePrimBE :: Parser Int64)

instance LEON Bool where
    {-# INLINE encode #-}
    encode False = encodePrim @Word8 0
    encode True  = encodePrim @Word8 1
    {-# INLINE decode #-}
    decode = decodePrim @Word8 >>= \ case 0 -> return False
                                          _ -> return True

instance LEON Ordering where
    {-# INLINE encode #-}
    encode = encode @Word8 . fromOrd
      where
        fromOrd LT = 0
        fromOrd EQ = 1
        fromOrd GT = 2
    {-# INLINE decode #-}
    decode = decode @Word8 >>= toOrd
      where
        toOrd 0 = return LT
        toOrd 1 = return EQ
        toOrd _ = return GT

#define LE_INST(type) instance LEON type where \
    {-# INLINE encode #-}; \
    encode = encodePrimLE; \
    {-# INLINE decode #-}; \
    decode = decodePrimLE; \

LE_INST(Word16)
LE_INST(Word32)
LE_INST(Word64)
LE_INST(Int16)
LE_INST(Int32)
LE_INST(Int64)
LE_INST(Float)
LE_INST(Double)
LE_INST(Char)

#define BE_INST(type) instance LEON (BE type) where \
    {-# INLINE encode #-}; \
    encode = encodePrim; \
    {-# INLINE decode #-}; \
    decode = decodePrim; \

BE_INST(Word16)
BE_INST(Word32)
BE_INST(Word64)
BE_INST(Int16)
BE_INST(Int32)
BE_INST(Int64)
BE_INST(Float)
BE_INST(Double)
BE_INST(Char)

--------------------------------------------------------------------------------

instance LEON a => LEON (V.Vector a) where
    {-# INLINE encode #-}
    encode xs = do
        encode (V.length xs)
        mapM_ encode xs
    {-# INLINE decode #-}
    decode = do
        len <- decode @Int
        V.packN len <$> replicateM len decode

instance {-# OVERLAPPABLE #-} (Prim a, LEON a) => LEON (V.PrimVector a) where
    {-# INLINE encode #-}
    encode xs = do
        encode (V.length xs)
        mapM_ encode (V.unpack xs)
    {-# INLINE decode #-}
    decode = do
        len <- decode @Int
        V.packN len <$> replicateM len decode

instance {-# OVERLAPPING #-} LEON V.Bytes where
    {-# INLINE encode #-}
    encode bs = do
        let l = V.length bs
        encode l
        B.bytes bs
    {-# INLINE decode #-}
    decode = decode @Int >>= P.take

instance LEON T.Text where
    {-# INLINE encode #-}
    encode (T.Text bs) = do
        let l = V.length bs
        encode l
        B.bytes bs
    {-# INLINE decode #-}
    decode = do
        l <- decode @Int
        T.Text <$> P.take l

instance LEON CBytes.CBytes where
    {-# INLINE encode #-}
    encode = encode . CBytes.toBytes
    {-# INLINE decode #-}
    decode = CBytes.fromBytes <$> decode

--------------------------------------------------------------------------------
-- Instances for list and a few tuples
--
instance LEON a => LEON [a] where
    {-# INLINE encode #-}
    encode xs = do
        encode (List.length xs)
        mapM_ encode xs
    {-# INLINE decode #-}
    decode = do
        len <- decode @Int
        replicateM len decode

instance LEON () where
    {-# INLINE encode #-}
    encode ()  = return ()
    {-# INLINE decode #-}
    decode     = return ()

instance (LEON a, LEON b) => LEON (a,b) where
    {-# INLINE encode #-}
    encode (a,b)           = encode a >> encode b
    {-# INLINE decode #-}
    decode                 = liftM2 (,) decode decode

instance (LEON a, LEON b, LEON c) => LEON (a,b,c) where
    {-# INLINE encode #-}
    encode (a,b,c)         = encode a >> encode b >> encode c
    {-# INLINE decode #-}
    decode                 = liftM3 (,,) decode decode decode

instance (LEON a, LEON b, LEON c, LEON d) => LEON (a,b,c,d) where
    {-# INLINE encode #-}
    encode (a,b,c,d)       = encode a >> encode b >> encode c >> encode d
    {-# INLINE decode #-}
    decode                 = liftM4 (,,,) decode decode decode decode

instance (LEON a, LEON b, LEON c, LEON d, LEON e) => LEON (a,b,c,d,e) where
    {-# INLINE encode #-}
    encode (a,b,c,d,e)     = encode a >> encode b >> encode c >> encode d >> encode e
    {-# INLINE decode #-}
    decode                 = liftM5 (,,,,) decode decode decode decode decode

--
-- and now just recurse:
--

instance (LEON a, LEON b, LEON c, LEON d, LEON e, LEON f)
        => LEON (a,b,c,d,e,f) where
    {-# INLINE encode #-}
    encode (a,b,c,d,e,f)   = encode (a,(b,c,d,e,f))
    {-# INLINE decode #-}
    decode                 = do (a,(b,c,d,e,f)) <- decode ; return (a,b,c,d,e,f)

instance (LEON a, LEON b, LEON c, LEON d, LEON e, LEON f, LEON g)
        => LEON (a,b,c,d,e,f,g) where
    {-# INLINE encode #-}
    encode (a,b,c,d,e,f,g) = encode (a,(b,c,d,e,f,g))
    {-# INLINE decode #-}
    decode                 = do (a,(b,c,d,e,f,g)) <- decode ; return (a,b,c,d,e,f,g)

instance (LEON a, LEON b, LEON c, LEON d, LEON e,
          LEON f, LEON g, LEON h)
        => LEON (a,b,c,d,e,f,g,h) where
    {-# INLINE encode #-}
    encode (a,b,c,d,e,f,g,h) = encode (a,(b,c,d,e,f,g,h))
    {-# INLINE decode #-}
    decode                   = do (a,(b,c,d,e,f,g,h)) <- decode ; return (a,b,c,d,e,f,g,h)

instance (LEON a, LEON b, LEON c, LEON d, LEON e,
          LEON f, LEON g, LEON h, LEON i)
        => LEON (a,b,c,d,e,f,g,h,i) where
    {-# INLINE encode #-}
    encode (a,b,c,d,e,f,g,h,i) = encode (a,(b,c,d,e,f,g,h,i))
    {-# INLINE decode #-}
    decode                     = do (a,(b,c,d,e,f,g,h,i)) <- decode ; return (a,b,c,d,e,f,g,h,i)

instance (LEON a, LEON b, LEON c, LEON d, LEON e,
          LEON f, LEON g, LEON h, LEON i, LEON j)
        => LEON (a,b,c,d,e,f,g,h,i,j) where
    {-# INLINE encode #-}
    encode (a,b,c,d,e,f,g,h,i,j) = encode (a,(b,c,d,e,f,g,h,i,j))
    {-# INLINE decode #-}
    decode                       = do (a,(b,c,d,e,f,g,h,i,j)) <- decode ; return (a,b,c,d,e,f,g,h,i,j)


------------------------------------------------------------------------
-- Container types

instance LEON a => LEON (Identity a) where
    {-# INLINE encode #-}
    encode (Identity x) = encode x
    {-# INLINE decode #-}
    decode = Identity <$> decode

instance (LEON a) => LEON (Maybe a) where
    {-# INLINE encode #-}
    encode Nothing  = encode @Word8 0
    encode (Just x) = encode @Word8 1 >> encode x
    {-# INLINE decode #-}
    decode = do
        w <- decode @Word8
        case w of
            0 -> return Nothing
            _ -> fmap Just decode

instance (LEON a, LEON b) => LEON (Either a b) where
    {-# INLINE encode #-}
    encode (Left  a) = encode @Word8 0 >> encode a
    encode (Right b) = encode @Word8 1 >> encode b
    {-# INLINE decode #-}
    decode = do
        w <- decode @Word8
        case w of
            0 -> fmap Left  decode
            _ -> fmap Right decode

--------------------------------------------------------------------------------
-- Portable, and pretty efficient, serialisation of Integer
--

-- Fold and unfold an Integer to and from a list of its bytes along with list's len
unroll :: (Integral a, Bits a) => a -> (Int, [Word8])
unroll = go 0 []
  where
    go !l ws !n
        | n == 0 = (l, List.reverse ws) -- little endian
        | otherwise = go (l+1) (fromIntegral n: ws) (n `shiftR` 8)


-- Build an Integer from a list of its bytes
roll :: (Integral a, Bits a) => [Word8] -> a
roll   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

-- Fixed-size type for a subset of Integer
type SmallInt = Int32

-- Integers are encoded in two ways: if they fit inside a SmallInt,
-- they're written as a byte tag, and that value.  If the Integer value
-- is too large to fit in a SmallInt, it is written as a byte array,
-- along with a sign and length field.
instance LEON Integer where
    {-# INLINE encode #-}
    encode n
        | n >= lo && n <= hi = do
            encode @Word8 0
            encode (fromIntegral n :: SmallInt)  -- fast path
        | otherwise = do
            encode @Word8 1
            encode sign
            -- unroll the bytes
            let (len, ws) = unroll (abs n)
            encode len
            mapM_ encode ws
     where
        lo = fromIntegral (minBound :: SmallInt) :: Integer
        hi = fromIntegral (maxBound :: SmallInt) :: Integer
        sign = fromIntegral (signum n) :: Word8
    {-# INLINE decode #-}
    decode = do
        tag <- decode @Word8
        case tag of
            0 -> fromIntegral <$> decode @SmallInt
            _ -> do sign  <- decode @Word8
                    bytes <- decode
                    let v = roll bytes
                    return $! if sign == 1 then v else - v

-- Fixed-size type for a subset of Natural
type NaturalWord = Word64
instance LEON Natural where
    {-# INLINE encode #-}
    encode n
        | n <= hi = do
            encode @Word8 0
            encode (fromIntegral n :: NaturalWord)  -- fast path
        | otherwise = do
            encode @Word8 1
            let (len, ws) = unroll (abs n)
            encode len
            mapM_ encode ws         -- unroll the bytes
      where
        hi = fromIntegral (maxBound :: NaturalWord) :: Natural
    {-# INLINE decode #-}
    decode = do
        tag <- decode :: Parser Word8
        case tag of
            0 -> fromIntegral <$> (decode :: Parser NaturalWord)
            _ -> do bytes <- decode
                    return $! roll bytes

------------------------------------------------------------------------
-- Fingerprints

-- | /Since: 0.7.6.0/
instance LEON Fingerprint where
    {-# INLINE encode #-}
    encode (Fingerprint x1 x2) = encode x1 >> encode x2
    {-# INLINE decode #-}
    decode = do
        x1 <- decode
        x2 <- decode
        return $! Fingerprint x1 x2

------------------------------------------------------------------------
-- Version

-- | /Since: 0.8.0.0/
instance LEON Version where
    {-# INLINE encode #-}
    encode (Version br tags) = encode br >> encode tags
    {-# INLINE decode #-}
    decode = Version <$> decode <*> decode

------------------------------------------------------------------------
-- Data.Monoid datatypes

-- | /Since: 0.8.4.0/
#define NT_INST0(nt, getnt) instance LEON nt where \
    {-# INLINE decode #-}; \
    decode = fmap nt decode; \
    {-# INLINE encode #-}; \
    encode = encode . getnt

#define NT_INST1(nt, getnt) instance LEON a => LEON (nt a) where \
    {-# INLINE decode #-}; \
    decode = fmap nt decode; \
    {-# INLINE encode #-}; \
    encode = encode . getnt

#define NT_INST2(nt, getnt) instance LEON (f a) => LEON (nt f a) where \
    {-# INLINE decode #-}; \
    decode = fmap nt decode; \
    {-# INLINE encode #-}; \
    encode = encode . getnt

NT_INST1(Monoid.Dual    , Monoid.getDual)
NT_INST1(Monoid.Sum     , Monoid.getSum)
NT_INST1(Monoid.Product , Monoid.getProduct)
NT_INST1(Monoid.First   , Monoid.getFirst)
NT_INST1(Monoid.Last    , Monoid.getLast)
NT_INST0(Monoid.All     , Monoid.getAll)
NT_INST0(Monoid.Any     , Monoid.getAny)
NT_INST2(Monoid.Alt     , Monoid.getAlt)

NT_INST1(Semigroup.Min    , Semigroup.getMin)
NT_INST1(Semigroup.Max    , Semigroup.getMax)
NT_INST1(Semigroup.First  , Semigroup.getFirst)
NT_INST1(Semigroup.Last   , Semigroup.getLast)
NT_INST1(Semigroup.Option , Semigroup.getOption)

instance LEON m => LEON (Semigroup.WrappedMonoid m) where
    {-# INLINE decode #-}
    decode = fmap Semigroup.WrapMonoid decode
    {-# INLINE encode #-}
    encode = encode . Semigroup.unwrapMonoid

instance (LEON a, LEON b) => LEON (Semigroup.Arg a b) where
    {-# INLINE decode #-}
    decode                     = liftM2 Semigroup.Arg decode decode
    {-# INLINE encode #-}
    encode (Semigroup.Arg a b) = encode a >> encode b

------------------------------------------------------------------------
-- Non-empty lists

instance LEON a => LEON (NE.NonEmpty a) where
    {-# INLINE decode #-}
    decode = fmap NE.fromList decode
    {-# INLINE encode #-}
    encode = encode . NE.toList

--------------------------------------------------------------------------------

-- Type without constructors
instance GLEONEncode V1 where
    {-# INLINE gencode #-}
    gencode _ = pure ()

instance GLEONDecode V1 where
    {-# INLINE gdecode #-}
    gdecode   = return undefined

-- Constructor without arguments
instance GLEONEncode U1 where
    {-# INLINE gencode #-}
    gencode U1 = pure ()

instance GLEONDecode U1 where
    {-# INLINE gdecode #-}
    gdecode    = return U1

-- Product: constructor with parameters
instance (GLEONEncode a, GLEONEncode b) => GLEONEncode (a :*: b) where
    {-# INLINE gencode #-}
    gencode (x :*: y) = gencode x >> gencode y

instance (GLEONDecode a, GLEONDecode b) => GLEONDecode (a :*: b) where
    {-# INLINE gdecode #-}
    gdecode = (:*:) <$> gdecode <*> gdecode

-- Metadata (constructor name, etc)
instance GLEONEncode a => GLEONEncode (M1 i c a) where
    {-# INLINE gencode #-}
    gencode = gencode . unM1

instance GLEONDecode a => GLEONDecode (M1 i c a) where
    {-# INLINE gdecode #-}
    gdecode = M1 <$> gdecode

-- Constants, additional parameters, and rank-1 recursion
instance LEON a => GLEONEncode (K1 i a) where
    {-# INLINE gencode #-}
    gencode = encode . unK1

instance LEON a => GLEONDecode (K1 i a) where
    {-# INLINE gdecode #-}
    gdecode = K1 <$> decode

-- Borrowed from the cereal package.

-- The following GLEON instance for sums has support for serializing
-- types with up to 2^64-1 constructors. It will use the minimal
-- number of bytes needed to encode the constructor. For example when
-- a type has 2^8 constructors or less it will use a single byte to
-- encode the constructor. If it has 2^16 constructors or less it will
-- use two bytes, and so on till 2^64-1.

#define GUARD(WORD) (size - 1) <= fromIntegral (maxBound :: WORD)
#define PUTSUM(WORD) GUARD(WORD) = encodeSum (0 :: WORD) (fromIntegral size)
#define GETSUM(WORD) GUARD(WORD) = (decode :: Parser WORD) >>= checkGetSum (fromIntegral size)

instance ( GSumEncode  a, GSumEncode  b
         , SumSize    a, SumSize    b) => GLEONEncode (a :+: b) where
    {-# INLINE gencode #-}
    gencode | PUTSUM(Word8) | PUTSUM(Word16) | PUTSUM(Word32) | PUTSUM(Word64)
         | otherwise = sizeError "encode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)

instance ( GSumDecode  a, GSumDecode  b
         , SumSize    a, SumSize    b) => GLEONDecode (a :+: b) where
    {-# INLINE gdecode #-}
    gdecode | GETSUM(Word8) | GETSUM(Word16) | GETSUM(Word32) | GETSUM(Word64)
         | otherwise = sizeError "decode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)

sizeError :: Show size => String -> size -> error
sizeError s size =
    error $ "Can't " ++ s ++ " a type with " ++ show size ++ " constructors"

------------------------------------------------------------------------

checkGetSum :: (Ord word, Num word, Bits word, GSumDecode f)
            => word -> word -> Parser (f a)
{-# INLINE checkGetSum #-}
checkGetSum size code | code < size = decodeSum code size
                      | otherwise   = fail "Unknown encoding for constructor"

class GSumDecode f where
    decodeSum :: (Ord word, Num word, Bits word) => word -> word -> Parser (f a)

class GSumEncode f where
    encodeSum :: (Num w, Bits w, LEON w) => w -> w -> f a -> Builder ()

instance (GSumDecode a, GSumDecode b) => GSumDecode (a :+: b) where
    {-# INLINE decodeSum #-}
    decodeSum !code !size | code < sizeL = L1 <$> decodeSum code           sizeL
                          | otherwise    = R1 <$> decodeSum (code - sizeL) sizeR
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL

instance (GSumEncode a, GSumEncode b) => GSumEncode (a :+: b) where
    {-# INLINE encodeSum #-}
    encodeSum !code !size s = case s of
                             L1 x -> encodeSum code           sizeL x
                             R1 x -> encodeSum (code + sizeL) sizeR x
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL

instance GLEONDecode a => GSumDecode (C1 c a) where
    {-# INLINE decodeSum #-}
    decodeSum _ _ = gdecode

instance GLEONEncode a => GSumEncode (C1 c a) where
    {-# INLINE encodeSum #-}
    encodeSum !code _ x = encode code >> gencode x

------------------------------------------------------------------------

class SumSize f where
    sumSize :: Tagged f Word64

#ifdef HAS_DATA_KIND
newtype Tagged (s :: Type -> Type) b = Tagged {unTagged :: b}
#else
newtype Tagged (s :: * -> *)       b = Tagged {unTagged :: b}
#endif

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a Word64) +
                       unTagged (sumSize :: Tagged b Word64)

instance SumSize (C1 c a) where
    sumSize = Tagged 1
