{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Std.Data.Builder.Textual
  (
    Padding(..)
  , dec
  , decWith
  , hex
  , hexWith

  , FFFormat(..)
  , float
  , double
  , floatWith
  , doubleWith

  , char7
  , string7

  , char8
  , string8

  , charUtf8
  , stringUtf8
  , text
  , show
  ) where

import Std.Data.Builder.Base
import Std.Data.Text
import Data.Primitive.PrimArray (setPrimArray, writePrimArray)
import Data.Primitive.Addr (indexOffAddr)
import GHC.Float (FFFormat(..))

-- | Buidlers which are safely UTF-8 encoded, thus can be used to build
-- text directly.
--
-- Use 'textualBuilder' to use a 'TextualBuilder' as bytes 'Builder'.
newtype TextualBuilder a = TextualBuilder { textualBuilder :: Builder a }
    deriving (Functor, Applicative, Monad)

buildText :: TextualBuilder a -> Text
{-# INLINE buildText #-}
buildText = Text . buildBytes . textualBuilder

--------------------------------------------------------------------------------

-- | Integral formatting options.
--
data IFFormat = IFFormat
    { totalWidth :: Int         -- ^ total width, only effective with padding options
    , padding :: Padding        -- ^ padding options
    , postiveSign :: Bool       -- ^ show @+@ when the number is positive
    , hexPrefix :: HexPrefix    -- ^ '0x', '0X' or no hex prefix, only effective with hex formatting
    } deriving (Show, Eq, Ord)

data Padding = NoPadding | ZeroPadding | LeftSpacePadding | RightSpacePadding deriving (Show, Eq, Ord)
data HexPrefix = NoHexPrefix | LowerHexPrefix | UpperHexPrefix deriving (Show, Eq, Ord)


dec ::Interal a => a -> TextualBuilder ()
dec = decWith undefined

decWith :: Integral a
        => IFFormat
        -> a
        -> TextualBuilder ()

decWith :: Integral a => IFFormat -> a -> TextualBuilder ()
{-# RULES "decimal/Int8" decimal    = boundedDecimal :: IFFormat -> Int8    -> TextualBuilder () #-}
{-# RULES "decimal/Int" decimal     = boundedDecimal :: IFFormat -> Int     -> TextualBuilder () #-}
{-# RULES "decimal/Int16" decimal   = boundedDecimal :: IFFormat -> Int16   -> TextualBuilder () #-}
{-# RULES "decimal/Int32" decimal   = boundedDecimal :: IFFormat -> Int32   -> TextualBuilder () #-}
{-# RULES "decimal/Int64" decimal   = boundedDecimal :: IFFormat -> Int64   -> TextualBuilder () #-}
{-# RULES "decimal/Word" decimal    = positive       :: IFFormat -> Word    -> TextualBuilder () #-}
{-# RULES "decimal/Word8" decimal   = positive       :: IFFormat -> Word8   -> TextualBuilder () #-}
{-# RULES "decimal/Word16" decimal  = positive       :: IFFormat -> Word16  -> TextualBuilder () #-}
{-# RULES "decimal/Word32" decimal  = positive       :: IFFormat -> Word32  -> TextualBuilder () #-}
{-# RULES "decimal/Word64" decimal  = positive       :: IFFormat -> Word64  -> TextualBuilder () #-}
{-# RULES "decimal/Integer" decimal = integer 10     :: IFFormat -> Integer -> TextualBuilder () #-}
decWith = decimal (<= -128)
{-# INLINE[0] decimal #-}

boundedDecimal :: (Integral a, Bounded a) => IFFormat -> a ->TextualBuilder ()
{-# SPECIALIZE boundedDecimal :: Int   -> IFFormat -> TextualBuilder () #-}
{-# SPECIALIZE boundedDecimal :: Int8  -> IFFormat -> TextualBuilder () #-}
{-# SPECIALIZE boundedDecimal :: Int16 -> IFFormat -> TextualBuilder () #-}
{-# SPECIALIZE boundedDecimal :: Int32 -> IFFormat -> TextualBuilder () #-}
{-# SPECIALIZE boundedDecimal :: Int64 -> IFFormat -> TextualBuilder () #-}
boundedDecimal padding plus i = decimal padding plus (== minBound) i

decimal :: (Integral a) => (a -> Bool) -> IFFormat -> a -> TextualBuilder ()
{-# INLINE decimal #-}
decimal pred format@(IFFormat width padding _ _) i
    | i < 0 = TextualBuilder $
        if pred i
        then do
            let (q, r) = i `quotRem` 10
                !qq = -q
                !n = countDigits qq + 2     -- extra two bytes: minus and last digit
            if width > n
            then case padding of
                NoPadding -> writeN n $ normalLoop1 qq r
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n
                        writePrimArray marr off minus                  -- leading minus
                        setPrimArray marr (off+1) leadingN zero           -- leading zeros
                        posDecimal marr (off+1+leadingN) n qq       -- digits
                        writePrimArray marr (off+width-1) (i2w (-r))   -- last digit
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n
                        setPrimArray marr off leadingN space    -- leading spaces
                        normalLoop1 qq r marr (off+leadingN)    -- normal loop
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n
                        normalLoop1 qq r marr off               -- normal loop
                        setPrimArray marr (off+n-2) trailingN space   -- trailing spaces
            else writeN n $ normalLoop1 qq r
        else do
            let !j = -i
                !n = countDigits j + 1  -- extra byte: last digit
            if width > n
            then case padding of
                NoPadding -> writeN n $ normalLoop j
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n
                        writePrimArray marr off minus                  -- leading minus
                        setPrimArray marr (off+1) leadingN zero           -- leading zeros
                        posDecimal marr (off+1+leadingN) n j        -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n
                        setPrimArray marr off leadingN space    -- leading spaces
                        normalLoop j marr (off+leadingN)        -- normal loop
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n
                        normalLoop j marr off                   -- normal loop
                        setPrimArray marr (off+n-2) trailingN space   -- trailing spaces
            else writeN n $ normalLoop j
    | otherwise = positive format i
  where
    normalLoop1 qq marr off = do
        writePrimArray marr off minus
        posDecimal marr (off+1) n qq
    normalLoop2 qq r marr off = do
        writePrimArray marr off minus
        posDecimal marr (off+1) n qq
        writePrimArray marr (off+n+1) (i2w (-r))

positive :: (Integral a) => IFFormat -> a -> TextualBuilder ()
{-# SPECIALIZE positive :: IFFormat -> Int    -> TextualBuilder () #-}
{-# SPECIALIZE positive :: IFFormat -> Int8   -> TextualBuilder () #-}
{-# SPECIALIZE positive :: IFFormat -> Int16  -> TextualBuilder () #-}
{-# SPECIALIZE positive :: IFFormat -> Int32  -> TextualBuilder () #-}
{-# SPECIALIZE positive :: IFFormat -> Int64  -> TextualBuilder () #-}
{-# SPECIALIZE positive :: IFFormat -> Word   -> TextualBuilder () #-}
{-# SPECIALIZE positive :: IFFormat -> Word8  -> TextualBuilder () #-}
{-# SPECIALIZE positive :: IFFormat -> Word16 -> TextualBuilder () #-}
{-# SPECIALIZE positive :: IFFormat -> Word32 -> TextualBuilder () #-}
{-# SPECIALIZE positive :: IFFormat -> Word64 -> TextualBuilder () #-}
positive (IFFormat width padding ps _) i =
    let !n = countDigits i
    in TextualBuilder $
        if ps
            then let n' = n+1
                in if width > n'
                then case padding of
                    NoPadding -> writeN n' $ \ marr off -> do
                        writePrimArray marr off plus                     -- leading plus
                        posDecimal marr (off+1) n i
                    ZeroPadding ->
                        writeN width $ \marr off -> do
                            let !leadingN = width-n'
                            writePrimArray marr off plus                    -- leading plus
                            setPrimArray marr (off+1) leadingN zero         -- leading zeros
                            posDecimal marr (off+1+leadingN) n i            -- digits
                    LeftSpacePadding ->
                        writeN width $ \marr off -> do
                            let !leadingN = width-n'
                            setPrimArray marr off leadingN space    -- leading spaces
                            writePrimArray marr (off+1) plus        -- leading plus
                            posDecimal j marr (off+1+leadingN) n i  -- normal loop
                    RightSpacePadding ->
                        writeN width $ \marr off -> do
                            let !trailingN = width-n'
                            writePrimArray marr off plus                -- leading plus
                            posDecimal j marr (off+1) n i               -- normal loop
                            setPrimArray marr (off+n-1) trailingN space -- trailing spaces
                else TextualBuilder . writeN n $ \ marr off -> do
                    writePrimArray marr off plus                        -- leading plus
                    posDecimal marr (off+1) n i
            else writeN n $ \ marr off -> posDecimal marr off n i

posDecimal :: (Integral a)
           => forall s. A.MutablePrimArray s Word8 -> Int -> Int -> a -> ST s ()
{-# INLINE posDecimal #-}
posDecimal marr off0 ds = go (off0 + ds - 1)
  where
    go off v
        | v >= 100 = do
            let (q, r) = v `quotRem` 100
            write2 off r
            go (off - 2) q
        | v < 10    = writePrimArray marr off (i2w v)
        | otherwise = write2 off v
    write2 off i0 = do
        let i = fromIntegral i0; j = i + i
        writePrimArray marr off $ indexOffAddr digits (j + 1)
        writePrimArray marr (off - 1) $ indexOffAddr digits j

data T = T !Integer !Int

integer :: Int -> Integer -> Builder
#ifdef INTEGER_GMP
integer 10 (S# i#) = decimal (I# i#)
integer 16 (S# i#) = hexadecimal (I# i#)
#endif
integer base i
    | i < 0     = singleton '-' <> go (-i)
    | otherwise = go i
  where
    go n | n < maxInt = int (fromInteger n)
         | otherwise  = putH (splitf (maxInt * maxInt) n)

    splitf p n
      | p > n       = [n]
      | otherwise   = splith p (splitf (p*p) n)

    splith p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) | q > 0     -> q : r : splitb p ns
                                  | otherwise -> r : splitb p ns
    splith _ _      = error "splith: the impossible happened."

    splitb p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) -> q : r : splitb p ns
    splitb _ _      = []

    T maxInt10 maxDigits10 =
        until ((>mi) . (*10) . fstT) (\(T n d) -> T (n*10) (d+1)) (T 10 1)
      where mi = fromIntegral (maxBound :: Int)
    T maxInt16 maxDigits16 =
        until ((>mi) . (*16) . fstT) (\(T n d) -> T (n*16) (d+1)) (T 16 1)
      where mi = fromIntegral (maxBound :: Int)

    fstT (T a _) = a

    maxInt | base == 10 = maxInt10
           | otherwise  = maxInt16
    maxDigits | base == 10 = maxDigits10
              | otherwise  = maxDigits16

    putH (n:ns) = case n `quotRemInteger` maxInt of
                    PAIR(x,y)
                        | q > 0     -> int q <> pblock r <> putB ns
                        | otherwise -> int r <> putB ns
                        where q = fromInteger x
                              r = fromInteger y
    putH _ = error "putH: the impossible happened"

    putB (n:ns) = case n `quotRemInteger` maxInt of
                    PAIR(x,y) -> pblock q <> pblock r <> putB ns
                        where q = fromInteger x
                              r = fromInteger y
    putB _ = Data.Monoid.mempty

    int :: Int -> Builder
    int x | base == 10 = decimal x
          | otherwise  = hexadecimal x

    pblock = loop maxDigits
      where
        loop !d !n
            | d == 1    = hexDigit n
            | otherwise = loop (d-1) q <> hexDigit r
            where q = n `quotInt` base
                  r = n `remInt` base

--------------------------------------------------------------------------------

digits :: Addr
digits = Addr "0001020304050607080910111213141516171819\
              \2021222324252627282930313233343536373839\
              \4041424344454647484950515253545556575859\
              \6061626364656667686970717273747576777879\
              \8081828384858687888990919293949596979899"#

countDigits :: (Integral a) => a -> Int
{-# INLINE countDigits #-}
countDigits v0
  | fromIntegral v64 == v0 = go 1 v64
  | otherwise              = goBig 1 (fromIntegral v0)
  where v64 = fromIntegral v0
        goBig !k (v :: Integer)
           | v > big   = goBig (k + 19) (v `quot` big)
           | otherwise = go k (fromIntegral v)
        big = 10000000000000000000
        go !k (v :: Word64)
           | v < 10    = k
           | v < 100   = k + 1
           | v < 1000  = k + 2
           | v < 1000000000000 =
               k + if v < 100000000
                   then if v < 1000000
                        then if v < 10000
                             then 3
                             else 4 + fin v 100000
                        else 6 + fin v 10000000
                   else if v < 10000000000
                        then 8 + fin v 1000000000
                        else 10 + fin v 100000000000
           | otherwise = go (k + 12) (v `quot` 1000000000000)
        fin v n = if v >= n then 1 else 0

minus, plus, zero, space :: Word8
{-# INLINE plus #-}
{-# INLINE minus #-}
{-# INLINE zero #-}
{-# INLINE space #-}
plus = 43
minus = 45
zero = 48
space = 32

i2w :: (Integral a) => a -> Word8
{-# INLINE i2w #-}
i2w v = zero + fromIntegral v

--------------------------------------------------------------------------------

float :: Float -> TextualBuilder ()
float = floatWith FFGeneric
double :: Double -> TextualBuilder ()
double = doubleWith FFGeneric

floatWith :: FFFormat -> Float -> TextualBuilder ()
floatWith = undefined
doubleWith :: FFFormat -> Float -> TextualBuilder ()
doubleWith = undefined

char7 :: Char -> TextualBuilder ()
char7 = undefined
string7 :: String -> TextualBuilder ()
string7 = undefined

char8 :: Char -> TextualBuilder ()
char8 = undefined
string8 :: String -> TextualBuilder ()
string8 = undefined

char :: Char -> TextualBuilder ()
char = undefined
string :: String -> TextualBuilder ()
string = undefined

text :: Text -> TextualBuilder ()
text = undefined
show :: Show a => a -> TextualBuilder ()
show = undefined
