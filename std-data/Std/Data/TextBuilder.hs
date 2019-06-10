{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveAnyClass             #-}

{-|
Module      : Std.Data.TextBuilder
Description : UTF8 compatible builders.
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Base on UTF8 compatible textual builders from 'Std.Data.Builder', we provide a newtype wrapper
'TextBuilder' which can be directly used to build 'Text'.

We also provide faster alternative to 'Show' class, i.e. 'ToText', which also provides 'Generic'
based instances deriving.

-}

module Std.Data.TextBuilder
  ( -- * ToText class
  ToText(..), toText, toBuilder, toBytes, toString
  -- * Str newtype
  , Str(..)
  -- * Textual Builder
  , TextBuilder
  , getBuilder
  , unsafeFromBuilder
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
  -- * Builder helpers
  , paren, parenWhen, curly, square, angle, quotes, squotes, colon, comma, intercalateVec, intercalateList
  ) where

import           Control.Monad
import qualified Data.Scientific          as Sci
import           Data.String
import           Data.Bits
import           Data.Data                (Data(..))
import           Data.Fixed
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Functor.Sum
import           Data.Int
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.Monoid                  as Monoid
import           Data.Proxy                   (Proxy (..))
import           Data.Ratio                   (Ratio, numerator, denominator)
import           Data.Tagged                  (Tagged (..))
import           Data.Word
import qualified Data.Semigroup               as Semigroup
import           Data.Typeable
import           GHC.Natural
import           GHC.Generics
import           Data.Version
import           Data.Primitive.Types
import qualified Std.Data.Builder         as B
import qualified Std.Data.Text.Base       as T
import           Std.Data.Text.Base       (Text(..))
import qualified Std.Data.Vector.Base     as V
import           Text.Read                (Read(..))
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))

-- | Buidlers which guarantee UTF-8 encoding, thus can be used to build
-- text directly.
--
-- Notes on 'IsString' instance: It's recommended to use 'IsString' instance, there's a rewrite rule to
-- turn encoding loop into a memcpy, which is much faster (the same rule also apply to 'stringUTF8').
-- Different from @Builder ()@, @TextBuilder ()@'s 'IsString' instance will give you desired UTF8 guarantees:
--
-- * @\NUL@ will be written directly as @\x00@.
--
-- * @\xD800@ ~ @\xDFFF@ will be replaced by replacement char.
--
newtype TextBuilder a = TextBuilder { getBuilder :: B.Builder a }
    deriving newtype (Functor, Applicative, Monad)

deriving newtype instance Semigroup (TextBuilder ())
deriving newtype instance Monoid (TextBuilder ())

instance (a ~ ()) => IsString (TextBuilder a) where
    {-# INLINE fromString #-}
    fromString = TextBuilder <$> B.stringUTF8

instance Arbitrary (TextBuilder ()) where
    arbitrary = TextBuilder . B.text <$> arbitrary
    shrink b = TextBuilder . B.text <$> shrink (buildText b)

instance CoArbitrary (TextBuilder ()) where
    coarbitrary = coarbitrary . buildText

instance Show (TextBuilder a) where
    show = show . buildText

instance ToText (TextBuilder a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ b = quotes (void b)

-- | Build a 'Text' using 'TextBuilder', which provide UTF-8 encoding guarantee.
buildText :: TextBuilder a -> Text
{-# INLINE buildText #-}
buildText = Text . B.buildBytes . getBuilder

-- | Unsafely turn a 'B.Builder' into 'TextBuilder', thus it's user's responsibility to
-- ensure only UTF-8 complied bytes are written.
unsafeFromBuilder :: B.Builder a -> TextBuilder a
{-# INLINE unsafeFromBuilder #-}
unsafeFromBuilder = TextBuilder

--------------------------------------------------------------------------------

-- | Turn 'String' into 'TextBuilder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's. This function will be rewritten into a memcpy if possible, (running a fast UTF-8 validation at runtime first).
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

-- | Write UTF8 encoded 'T.Text' using 'Builder'.
--
-- Note, if you're trying to write string literals builders,
-- please open 'OverloadedStrings' and use 'Builder's 'IsString' instance,
-- it will be rewritten into a memcpy.
text :: T.Text -> TextBuilder ()
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

--------------------------------------------------------------------------------

-- | add @(...)@ to original builder.
paren :: TextBuilder () -> TextBuilder ()
{-# INLINE paren #-}
paren (TextBuilder b) = TextBuilder (B.paren b)

-- | Add "(..)" around builders when condition is met, otherwise add nothing.
--
-- This is useful when defining 'ToText' instances.
parenWhen :: Bool -> TextBuilder () -> TextBuilder ()
{-# INLINE parenWhen #-}
parenWhen True b = paren b
parenWhen _    b = b

-- | add @{...}@ to original builder.
curly :: TextBuilder () -> TextBuilder ()
{-# INLINE curly #-}
curly (TextBuilder b) = TextBuilder (B.curly b)

-- | add @[...]@ to original builder.
square :: TextBuilder () -> TextBuilder ()
{-# INLINE square #-}
square (TextBuilder b) = TextBuilder (B.square b)

-- | add @<...>@ to original builder.
angle :: TextBuilder () -> TextBuilder ()
{-# INLINE angle #-}
angle (TextBuilder b) = TextBuilder (B.angle b)

-- | add @"..."@ to original builder.
quotes :: TextBuilder () -> TextBuilder ()
{-# INLINE quotes #-}
quotes (TextBuilder b) = TextBuilder (B.quotes b)

-- | add @'...'@ to original builder.
squotes :: TextBuilder () -> TextBuilder ()
{-# INLINE squotes #-}
squotes (TextBuilder b) = TextBuilder (B.squotes b)

-- | write an ASCII @:@
colon ::  TextBuilder ()
{-# INLINE colon #-}
colon = TextBuilder B.colon

-- | write an ASCII @,@
comma ::  TextBuilder ()
{-# INLINE comma #-}
comma = TextBuilder B.comma

-- | Use separator to connect a vector of builders.
intercalateVec :: (V.Vec v a)
               => TextBuilder ()            -- ^ the seperator
               -> (a -> TextBuilder ())     -- ^ value formatter
               -> v a                       -- ^ value list
               ->  TextBuilder ()
{-# INLINE intercalateVec #-}
intercalateVec (TextBuilder s) f = TextBuilder . B.intercalateVec s (getBuilder . f)

-- | Use separator to connect a list of builders.
intercalateList :: TextBuilder ()           -- ^ the seperator
                -> (a -> TextBuilder ())    -- ^ value formatter
                -> [a]                      -- ^ value vector
                -> TextBuilder ()
{-# INLINE intercalateList #-}
intercalateList (TextBuilder s) f = TextBuilder . B.intercalateList s (getBuilder . f)

--------------------------------------------------------------------------------
-- | Newtype wrapper for @[Char]@ to provide textual instances.
--
-- To encourage using 'Text' as the textual representation, we didn't provide special
-- treatment to differentiate instances between @[a]@ and @[Char]@ in various places.
-- This newtype is therefore to provide instances similar to @T.Text@, in case you really
-- need to wrap a 'String'.
newtype Str = Str { chrs :: [Char] } deriving stock (Eq, Ord, Data, Typeable, Generic)

instance Show Str where show = show . chrs
instance Read Str where readPrec = Str <$> readPrec

instance ToText Str where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = TextBuilder . B.string8 . show

--------------------------------------------------------------------------------
-- Data types
--
-- | A class similar to 'Show', serving the purpose that quickly convert a data type
-- to a 'Text' value.
class ToText a where
    toTextBuilder :: Int -> a  -> TextBuilder ()
    default toTextBuilder :: (Generic a, GToText (Rep a)) => Int -> a -> TextBuilder ()
    toTextBuilder p = gToTextBuilder p . from

class GToText f where
    gToTextBuilder :: Int -> f a -> TextBuilder ()


class GFieldToText f where
    gFieldToTextBuilder :: B.Builder () -> Int -> f a -> B.Builder ()

instance (GFieldToText a, GFieldToText b) => GFieldToText (a :*: b) where
    {-# INLINE gFieldToTextBuilder #-}
    gFieldToTextBuilder sep p (a :*: b) =
        gFieldToTextBuilder sep p a >> sep >> gFieldToTextBuilder sep p b

instance (GToText f) => GFieldToText (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFieldToTextBuilder #-}
    gFieldToTextBuilder _ p (M1 x) = getBuilder (gToTextBuilder p x)

instance (GToText f, Selector (MetaSel (Just l) u ss ds)) => GFieldToText (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gFieldToTextBuilder #-}
    gFieldToTextBuilder _ _ m1@(M1 x) =
        B.stringModifiedUTF8 (selName m1) >> " = " >> getBuilder (gToTextBuilder 0 x)

instance GToText V1 where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder _ = error "Std.Data.TextBuilder: empty data type"

instance (GToText f, GToText g) => GToText (f :+: g) where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder p (L1 x) = gToTextBuilder p x
    gToTextBuilder p (R1 x) = gToTextBuilder p x

-- | Constructor without payload, convert to String
instance (Constructor c) => GToText (C1 c U1) where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder _ m1 =
        TextBuilder . B.stringModifiedUTF8 $ conName m1

-- | Constructor with payloads
instance (GFieldToText (S1 sc f), Constructor c) => GToText (C1 c (S1 sc f)) where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder p m1@(M1 x) =
        parenWhen (p > 10) . TextBuilder $ do
            B.stringModifiedUTF8 $ conName m1
            B.char8 ' '
            if conIsRecord m1
            then B.curly $ gFieldToTextBuilder (B.char7 ',' >> B.char7 ' ') p x
            else gFieldToTextBuilder (B.char7 ' ') 11 x

instance (GFieldToText (a :*: b), Constructor c) => GToText (C1 c (a :*: b)) where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder p m1@(M1 x) =
        case conFixity m1 of
            Prefix -> parenWhen (p > 10) . TextBuilder $ do
                B.stringModifiedUTF8 $ conName m1
                B.char8 ' '
                if conIsRecord m1
                then B.curly $ gFieldToTextBuilder (B.char7 ',' >> B.char7 ' ') p x
                else gFieldToTextBuilder (B.char7 ' ') 11 x
            Infix _ p' -> parenWhen (p > p') . TextBuilder $ do
                gFieldToTextBuilder
                    (B.char8 ' ' >> B.stringModifiedUTF8 (conName m1) >> B.char8 ' ') (p'+1) x

instance ToText a => GToText (K1 i a) where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder p (K1 x) = toTextBuilder p x

--------------------------------------------------------------------------------
-- Data types
instance GToText f => GToText (D1 c f) where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder p (M1 x) = gToTextBuilder p x


-- | Directly convert data to 'Text'.
toText :: ToText a => a -> Text
{-# INLINE toText #-}
toText = buildText .  toTextBuilder 0

-- | Directly convert data to 'B.Builder'.
toBuilder :: ToText a => a -> B.Builder ()
{-# INLINE toBuilder #-}
toBuilder = getBuilder . toTextBuilder 0

-- | Directly convert data to 'V.Bytes'.
toBytes :: ToText a => a -> V.Bytes
{-# INLINE toBytes #-}
toBytes = B.buildBytes .  toBuilder

-- | Faster 'show' replacement.
toString :: ToText a => a -> String
{-# INLINE toString #-}
toString = T.unpack . toText

instance ToText Bool where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ True = TextBuilder "True"
    toTextBuilder _ _    = TextBuilder "False"

instance ToText Char where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = TextBuilder . B.string8 . show

instance ToText Double where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = double;}
instance ToText Float  where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = float;}

instance ToText Int     where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ToText Int8    where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ToText Int16   where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ToText Int32   where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ToText Int64   where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ToText Word     where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ToText Word8    where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ToText Word16   where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ToText Word32   where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ToText Word64   where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}

instance ToText Integer  where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = integer;}
instance ToText Natural  where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = integer . fromIntegral}
instance ToText Ordering where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ GT = TextBuilder "GT"
    toTextBuilder _ EQ = TextBuilder "EQ"
    toTextBuilder _ _  = TextBuilder "LT"

instance ToText () where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ () = TextBuilder "()"

instance ToText Version where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = stringUTF8 . show

-- | To keep sync with 'Show' instance's escaping rule, we reuse show here, so it won't be as fast as memcpy.
instance ToText Text where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = stringUTF8 . show

instance ToText Sci.Scientific where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = scientific

instance ToText a => ToText [a] where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = square . intercalateList comma (toTextBuilder 0)

instance ToText a => ToText (V.Vector a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = square . intercalateVec comma (toTextBuilder 0)

instance (Prim a, ToText a) => ToText (V.PrimVector a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = square . intercalateVec comma (toTextBuilder 0)

instance (ToText a, ToText b) => ToText (a, b) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (a, b) = paren $  toTextBuilder 0 a
                     >> comma >> toTextBuilder 0 b

instance (ToText a, ToText b, ToText c) => ToText (a, b, c) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (a, b, c) = paren $  toTextBuilder 0 a
                     >> comma >> toTextBuilder 0 b
                     >> comma >> toTextBuilder 0 c

instance (ToText a, ToText b, ToText c, ToText d) => ToText (a, b, c, d) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (a, b, c, d) = paren $  toTextBuilder 0 a
                     >> comma >> toTextBuilder 0 b
                     >> comma >> toTextBuilder 0 c
                     >> comma >> toTextBuilder 0 d

instance (ToText a, ToText b, ToText c, ToText d, ToText e) => ToText (a, b, c, d, e) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (a, b, c, d, e) = paren $  toTextBuilder 0 a
                     >> comma >> toTextBuilder 0 b
                     >> comma >> toTextBuilder 0 c
                     >> comma >> toTextBuilder 0 d
                     >> comma >> toTextBuilder 0 e

instance (ToText a, ToText b, ToText c, ToText d, ToText e, ToText f) => ToText (a, b, c, d, e, f) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (a, b, c, d, e, f) = paren $  toTextBuilder 0 a
                     >> comma >> toTextBuilder 0 b
                     >> comma >> toTextBuilder 0 c
                     >> comma >> toTextBuilder 0 d
                     >> comma >> toTextBuilder 0 e
                     >> comma >> toTextBuilder 0 f

instance (ToText a, ToText b, ToText c, ToText d, ToText e, ToText f, ToText g) => ToText (a, b, c, d, e, f, g) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (a, b, c, d, e, f, g) = paren $  toTextBuilder 0 a
                     >> comma >> toTextBuilder 0 b
                     >> comma >> toTextBuilder 0 c
                     >> comma >> toTextBuilder 0 d
                     >> comma >> toTextBuilder 0 e
                     >> comma >> toTextBuilder 0 f
                     >> comma >> toTextBuilder 0 g

instance ToText a => ToText (Maybe a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder p (Just x) = parenWhen (p > 10) $ do TextBuilder "Just "
                                                       toTextBuilder 11 x
    toTextBuilder _ _        = TextBuilder "Nothing"

instance (ToText a, ToText b) => ToText (Either a b) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder p (Left x) = parenWhen (p > 10) $ do TextBuilder "Left "
                                                       toTextBuilder 11 x
    toTextBuilder p (Right x) = parenWhen (p > 10) $ do TextBuilder "Right "
                                                        toTextBuilder 11 x

instance (ToText a, Integral a) => ToText (Ratio a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder p r = parenWhen (p > 10) $ do toTextBuilder 8 (numerator r)
                                                TextBuilder " % "
                                                toTextBuilder 8 (denominator r)

instance HasResolution a => ToText (Fixed a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = TextBuilder . B.string8 .  show

deriving anyclass instance ToText a => ToText (Semigroup.Min a)
deriving anyclass instance ToText a => ToText (Semigroup.Max a)
deriving anyclass instance ToText a => ToText (Semigroup.First a)
deriving anyclass instance ToText a => ToText (Semigroup.Last a)
deriving anyclass instance ToText a => ToText (Semigroup.WrappedMonoid a)
deriving anyclass instance ToText a => ToText (Semigroup.Dual a)
deriving anyclass instance ToText a => ToText (Monoid.First a)
deriving anyclass instance ToText a => ToText (Monoid.Last a)
deriving anyclass instance ToText a => ToText (NonEmpty a)
deriving anyclass instance ToText a => ToText (Identity a)
deriving anyclass instance ToText a => ToText (Const a b)
deriving anyclass instance ToText (Proxy a)
deriving anyclass instance ToText b => ToText (Tagged a b)
deriving anyclass instance ToText (f (g a)) => ToText (Compose f g a)
deriving anyclass instance (ToText (f a), ToText (g a)) => ToText (Product f g a)
deriving anyclass instance (ToText (f a), ToText (g a), ToText a) => ToText (Sum f g a)
