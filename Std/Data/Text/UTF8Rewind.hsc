{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Std.Data.Text.UTF8Rewind
Description : Errno provided by libuv
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

INTERNAL MODULE, provides utf8rewind constants

-}

module Std.Data.Text.UTF8Rewind where

import Data.Bits
import Foreign.C.Types
import GHC.Generics

#include "utf8rewind.h"

-- | Locale for case mapping.
newtype Locale = Locale CSize deriving (Show, Eq, Ord, Generic)

#{enum Locale, Locale, localeDefault    = UTF8_LOCALE_DEFAULT }
#{enum Locale, Locale, localeLithuanian = UTF8_LOCALE_LITHUANIAN }
#{enum Locale, Locale, localeTurkishAndAzeriLatin = UTF8_LOCALE_TURKISH_AND_AZERI_LATIN }

-- | see 'NormalizeMode' in Std.Data.Text.Base
#{enum CSize, CSize, normalizeCompose       = UTF8_NORMALIZE_COMPOSE }
#{enum CSize, CSize, normalizeDecompose     = UTF8_NORMALIZE_DECOMPOSE }
#{enum CSize, CSize, normalizeCompatibility = UTF8_NORMALIZE_COMPATIBILITY }

{-|
These are the Unicode Normalization Forms:

@
Form                         | Description
---------------------------- | ---------------------------------------------
Normalization Form D (NFD)   | Canonical decomposition
Normalization Form C (NFC)   | Canonical decomposition, followed by canonical composition
Normalization Form KD (NFKD) | Compatibility decomposition
Normalization Form KC (NFKC) | Compatibility decomposition, followed by canonical composition
@ 
-}
data NormalizeMode = NFC | NFKC | NFD | NFKD deriving (Show, Eq, Ord, Generic)

normalizeModeToFlag :: NormalizeMode -> CSize
normalizeModeToFlag NFC  = #{const UTF8_NORMALIZE_COMPOSE}
normalizeModeToFlag NFKC = #{const UTF8_NORMALIZE_COMPOSE} + #{const UTF8_NORMALIZE_COMPATIBILITY}
normalizeModeToFlag NFD  = #{const UTF8_NORMALIZE_DECOMPOSE}
normalizeModeToFlag NFKD = #{const UTF8_NORMALIZE_DECOMPOSE} + #{const UTF8_NORMALIZE_COMPATIBILITY}

data NormalizationResult = NormalizedYes | NormalizedMaybe | NormalizedNo deriving (Show, Eq, Ord, Generic)

toNormalizationResult :: Int -> NormalizationResult
toNormalizationResult #{const UTF8_NORMALIZATION_RESULT_YES} = NormalizedYes
toNormalizationResult #{const UTF8_NORMALIZATION_RESULT_MAYBE} = NormalizedMaybe
toNormalizationResult #{const UTF8_NORMALIZATION_RESULT_NO} = NormalizedNo


-- | Unicode categories.
-- See 'Std.Data.Text.Base.isCategory', you can combine categories with bitwise or.
newtype Category = Category CSize deriving (Show, Eq, Ord, Bits, FiniteBits, Generic)

#{enum Category, Category, categoryLetterUppercase        = UTF8_CATEGORY_LETTER_UPPERCASE }
#{enum Category, Category, categoryLetterLowercase        = UTF8_CATEGORY_LETTER_LOWERCASE }
#{enum Category, Category, categoryLetterTitlecase        = UTF8_CATEGORY_LETTER_TITLECASE }
#{enum Category, Category, categoryLetterOther            = UTF8_CATEGORY_LETTER_OTHER }
#{enum Category, Category, categoryLetter                 = UTF8_CATEGORY_LETTER }
#{enum Category, Category, categoryCaseMapped             = UTF8_CATEGORY_CASE_MAPPED }

#{enum Category, Category, categoryMarkNonSpacing         = UTF8_CATEGORY_MARK_NON_SPACING }
#{enum Category, Category, categoryMarkSpacing            = UTF8_CATEGORY_MARK_SPACING }
#{enum Category, Category, categoryMarkEnclosing          = UTF8_CATEGORY_MARK_ENCLOSING }
#{enum Category, Category, categoryMark                   = UTF8_CATEGORY_MARK }

#{enum Category, Category, categoryNumberDecimal          = UTF8_CATEGORY_NUMBER_DECIMAL }
#{enum Category, Category, categoryNumberLetter           = UTF8_CATEGORY_NUMBER_LETTER }
#{enum Category, Category, categoryNumberOther            = UTF8_CATEGORY_NUMBER_OTHER }
#{enum Category, Category, categoryNumber                 = UTF8_CATEGORY_NUMBER }

#{enum Category, Category, categoryPunctuationConnector   = UTF8_CATEGORY_PUNCTUATION_CONNECTOR }
#{enum Category, Category, categoryPunctuationDash        = UTF8_CATEGORY_PUNCTUATION_DASH }
#{enum Category, Category, categoryPunctuationOpen        = UTF8_CATEGORY_PUNCTUATION_OPEN }
#{enum Category, Category, categoryPunctuationClose       = UTF8_CATEGORY_PUNCTUATION_CLOSE }
#{enum Category, Category, categoryPunctuationInitial     = UTF8_CATEGORY_PUNCTUATION_INITIAL }
#{enum Category, Category, categoryPunctuationFinal       = UTF8_CATEGORY_PUNCTUATION_FINAL }
#{enum Category, Category, categoryPunctuationOther       = UTF8_CATEGORY_PUNCTUATION_OTHER }
#{enum Category, Category, categoryPunctuation            = UTF8_CATEGORY_PUNCTUATION }

#{enum Category, Category, categorySymbolMath             = UTF8_CATEGORY_SYMBOL_MATH }
#{enum Category, Category, categorySymbolCurrency         = UTF8_CATEGORY_SYMBOL_CURRENCY }
#{enum Category, Category, categorySymbolModifier         = UTF8_CATEGORY_SYMBOL_MODIFIER }
#{enum Category, Category, categorySymbolOther            = UTF8_CATEGORY_SYMBOL_OTHER }
#{enum Category, Category, categorySymbol                 = UTF8_CATEGORY_SYMBOL }

#{enum Category, Category, categorySeparatorSpace         = UTF8_CATEGORY_SEPARATOR_SPACE }
#{enum Category, Category, categorySeparatorLine          = UTF8_CATEGORY_SEPARATOR_LINE }
#{enum Category, Category, categorySeparatorParagraph     = UTF8_CATEGORY_SEPARATOR_PARAGRAPH }
#{enum Category, Category, categorySeparator              = UTF8_CATEGORY_SEPARATOR }
#{enum Category, Category, categoryControl                = UTF8_CATEGORY_CONTROL }
#{enum Category, Category, categoryFormat                 = UTF8_CATEGORY_FORMAT }
#{enum Category, Category, categorySurrogate              = UTF8_CATEGORY_SURROGATE }
#{enum Category, Category, categoryPrivateUse             = UTF8_CATEGORY_PRIVATE_USE }
#{enum Category, Category, categoryUnassigned             = UTF8_CATEGORY_UNASSIGNED }
#{enum Category, Category, categoryCompatibility          = UTF8_CATEGORY_COMPATIBILITY }
#{enum Category, Category, categoryIgnoreGraphemeCluste   = UTF8_CATEGORY_IGNORE_GRAPHEME_CLUSTER }
#{enum Category, Category, categoryIscntrl                = UTF8_CATEGORY_ISCNTRL }

#{enum Category, Category, categoryIsprint                = UTF8_CATEGORY_ISPRINT }
#{enum Category, Category, categoryIsspace                = UTF8_CATEGORY_ISSPACE }
#{enum Category, Category, categoryIsblank                = UTF8_CATEGORY_ISBLANK }
#{enum Category, Category, categoryIsgraph                = UTF8_CATEGORY_ISGRAPH }
#{enum Category, Category, categoryIspunct                = UTF8_CATEGORY_ISPUNCT }
#{enum Category, Category, categoryIsalnum                = UTF8_CATEGORY_ISALNUM }
#{enum Category, Category, categoryIsalpha                = UTF8_CATEGORY_ISALPHA }
#{enum Category, Category, categoryIsupper                = UTF8_CATEGORY_ISUPPER }
#{enum Category, Category, categoryIslower                = UTF8_CATEGORY_ISLOWER }
#{enum Category, Category, categoryIsdigit                = UTF8_CATEGORY_ISDIGIT }
#{enum Category, Category, categoryIsxdigit               = UTF8_CATEGORY_ISXDIGIT }

foreign import ccall unsafe utf8envlocale :: IO Category
