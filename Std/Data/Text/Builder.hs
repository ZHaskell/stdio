{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Std.Data.Text.Builder
  (
  -- * Textual Builder
    TextBuilder(..)
  , buildText
  ) where

import Std.Data.Builder.Base    as B
import Std.Data.Builder.Numeric as B
import Data.String
import Std.Data.Text.Base              (Text(..))

-- | Buidlers which guarantee UTF-8 encoding, thus can be used to build
-- text directly.
--
newtype TextBuilder a = TextBuilder { toBuilder :: Builder a }
    deriving (Functor, Applicative, Monad)

instance (a ~ ()) => IsString (TextBuilder a) where
    {-# INLINE fromString #-}
    fromString = TextBuilder . fromString

buildText :: TextBuilder a -> Text
{-# INLINE buildText #-}
buildText = Text . B.buildBytes . toBuilder

--------------------------------------------------------------------------------

stringUTF8 :: String -> TextBuilder ()
{-# INLINE stringUTF8 #-}
stringUTF8 = TextBuilder . B.stringUTF8

charUTF8 :: Char -> TextBuilder ()
{-# INLINE charUTF8 #-}
charUTF8 = TextBuilder . B.charUTF8

string7 :: String -> TextBuilder ()
{-# INLINE string7 #-}
string7 = TextBuilder . B.string7

char7 :: Char -> TextBuilder ()
{-# INLINE char7 #-}
char7 = TextBuilder . B.char7

text :: Text -> TextBuilder ()
{-# INLINE text #-}
text = TextBuilder . B.text

int :: (Integral a, Bounded a) => a -> TextBuilder ()
{-# INLINE int #-}
int = TextBuilder . B.int

-- TODO, add more UTF8 compatible buidlers here
