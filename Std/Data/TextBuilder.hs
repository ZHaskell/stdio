{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}

{-|
Module      : Std.Data.Builder.Numeric
Description : UTF8 compatible builders.
Copyright   : (c) Winterland, 2017-2019
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

UTF8 compatible textual builders.

-}

module Std.Data.TextBuilder
  (
  -- * Textual Builder
    TextBuilder(..)
  , buildText
  -- * Basic UTF8 builders
  , stringUTF8, charUTF8, string7, char7, text
  -- * Numeric builders
  -- ** Integral type formatting
  , B.IFormat(..)
  , B.defaultIFormat
  , B.Padding(..)
  , int
  , intWith
  , integer
  -- ** Fixded size hexidecimal formatting
  , hex, heX
  -- ** IEEE float formating
  , B.FFormat(..)
  , double
  , doubleWith
  , float
  , floatWith
  , scientific
  , scientificWith
  ) where

import qualified Data.Scientific          as Sci
import           Data.String
import           Data.Bits
import qualified Std.Data.Builder.Base    as B
import qualified Std.Data.Builder.Numeric as B
import           Std.Data.Text.Base       (Text (..))

-- | Buidlers which guarantee UTF-8 encoding, thus can be used to build
-- text directly.
--
-- Notes on 'IsString' instance: It's recommended to use 'IsString' instance instead of 'stringUTF8'
-- or 'string7' since there's a rewrite rule to turn encoding loop into a memcpy, which is much faster.
--
-- The 'IsString' instance also gives desired UTF8 guarantees:
--
-- * "\NUL" will be written directly as @\x00@.
--
-- * @\xD800@ ~ @\xDFFF@ will be replaced by replacement char.
--
newtype TextBuilder a = TextBuilder { toBuilder :: B.Builder a }
    deriving (Functor, Applicative, Monad)

instance (a ~ ()) => IsString (TextBuilder a) where
    {-# INLINE fromString #-}
    fromString = TextBuilder . fromString

deriving instance Semigroup (TextBuilder ())
deriving instance Monoid (TextBuilder ())

buildText :: TextBuilder a -> Text
{-# INLINE buildText #-}
buildText = Text . B.buildBytes . toBuilder

--------------------------------------------------------------------------------

-- | Turn 'String' into 'TextBuilder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's.
--
-- Note, if you're trying to write string literals builders,
-- please open 'OverloadedStrings' and use 'TextBuilder''s 'IsString' instance,
-- it will be rewritten into a memcpy, instead of encoding 'Char's in a loop like
-- what 'stringUTF8' do.
stringUTF8 :: String -> TextBuilder ()
{-# INLINE stringUTF8 #-}
stringUTF8 = TextBuilder . B.stringUTF8

-- | Turn 'Char' into 'TextBuilder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's.
charUTF8 :: Char -> TextBuilder ()
{-# INLINE charUTF8 #-}
charUTF8 = TextBuilder . B.charUTF8

-- | Turn 'String' into 'TextBuilder' with ASCII7 encoding
--
-- Codepoints beyond @'\x7F'@ will be chopped.
string7 :: String -> TextBuilder ()
{-# INLINE string7 #-}
string7 = TextBuilder . B.string7

-- | Turn 'Char' into 'TextBuilder' with ASCII7 encoding
--
-- Codepoints beyond @'\x7F'@ will be chopped.
char7 :: Char -> TextBuilder ()
{-# INLINE char7 #-}
char7 = TextBuilder . B.char7

-- | Write UTF8 encoded 'Text' using 'Builder'.
--
-- Note, if you're trying to write string literals builders,
-- please open 'OverloadedStrings' and use 'Builder's 'IsString' instance,
-- it will be rewritten into a memcpy.
text :: Text -> TextBuilder ()
{-# INLINE text #-}
text = TextBuilder . B.text

--------------------------------------------------------------------------------

-- | @int = intWith defaultIFormat@
int :: (Integral a, Bounded a) => a -> TextBuilder ()
{-# INLINE int #-}
int = TextBuilder . B.int

-- | Format a 'Bounded' 'Integral' type like @Int@ or @Word16@ into decimal ascii digits.
intWith :: (Integral a, Bounded a)
        => B.IFormat
        -> a
        -> TextBuilder ()
{-# INLINE intWith #-}
intWith fmt x = TextBuilder $ B.intWith fmt x

-- | Format a 'Integer' into decimal ascii digits.
integer :: Integer -> TextBuilder ()
{-# INLINE integer #-}
integer = TextBuilder . B.integer

-- | Format a 'FiniteBits' 'Integral' type into hex nibbles.
hex :: (FiniteBits a, Integral a) => a -> TextBuilder ()
{-# INLINE hex #-}
hex = TextBuilder . B.hex

-- | The UPPERCASED version of 'hex'.
heX :: (FiniteBits a, Integral a) => a -> TextBuilder ()
{-# INLINE heX #-}
heX = TextBuilder . B.heX

-- | Decimal encoding of an IEEE 'Float'.
--
-- Using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
float :: Float -> TextBuilder ()
{-# INLINE float #-}
float = TextBuilder . B.float

-- | Format single-precision float using drisu3 with dragon4 fallback.
floatWith :: B.FFormat
          -> Maybe Int  -- ^ Number of decimal places to render.
          -> Float
          -> TextBuilder ()
{-# INLINE floatWith #-}
floatWith fmt ds x = TextBuilder (B.floatWith fmt ds x)


-- | Decimal encoding of an IEEE 'Double'.
--
-- Using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
double :: Double -> TextBuilder ()
{-# INLINE double #-}
double = TextBuilder . B.double

-- | Format double-precision float using drisu3 with dragon4 fallback.
doubleWith :: B.FFormat
           -> Maybe Int  -- ^ Number of decimal places to render.
           -> Double
           -> TextBuilder ()
{-# INLINE doubleWith #-}
doubleWith fmt ds x = TextBuilder (B.doubleWith fmt ds x)


-- | A @Builder@ which renders a scientific number to full
-- precision, using standard decimal notation for arguments whose
-- absolute value lies between @0.1@ and @9,999,999@, and scientific
-- notation otherwise.
scientific :: Sci.Scientific -> TextBuilder ()
{-# INLINE scientific #-}
scientific = TextBuilder . B.scientific

-- | Like 'scientific' but provides rendering options.
scientificWith :: B.FFormat
               -> Maybe Int  -- ^ Number of decimal places to render.
               -> Sci.Scientific
               -> TextBuilder ()
{-# INLINE scientificWith #-}
scientificWith fmt ds x = TextBuilder (B.scientificWith fmt ds x)
