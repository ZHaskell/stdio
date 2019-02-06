{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : Std.Data.Text
Description : Unicode text processing
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

A 'Text' simply wraps a 'Bytes' that are UTF-8 encoded codepoints, you can use 'validate' \/ 'validateMaybe' to construct a 'Text'.

-}

module Std.Data.Text (
  -- * Text type
    Text
  , validate, validateMaybe
  -- * Basic creating
  , empty, singleton, copy
  -- * Building text
  , replicate, cycleN
  -- * Conversion between list
  , pack, packN, packR, packRN
  , unpack, unpackR
  -- * Conversion between codepoint vector
  , fromVector
  , toVector
  -- * Basic interface
  , null
  , length
  , append
  , map', imap'
  , foldl', ifoldl'
  , foldr', ifoldr'
  , concat, concatMap
    -- ** Special folds
  , count, all, any
  -- * Searching by equality
  , elem, notElem
  -- * Slice manipulation
  , cons, snoc
  , uncons, unsnoc
  , headMaybe, tailMayEmpty
  , lastMaybe, initMayEmpty
  , inits, tails
  , take, drop, takeLast, dropLast
  , slice
  , splitAt
  , takeWhile, takeLastWhile, dropWhile, dropLastWhile, dropAround
  , break, span
  , breakEnd, spanEnd, breakOn
  , group, groupBy
  , stripPrefix, stripSuffix
  , split, splitWith, splitOn
  , isPrefixOf, isSuffixOf, isInfixOf
  , commonPrefix
  , words, lines, unwords, unlines
  , padLeft, padRight
  -- * Transform
  , reverse
  , intersperse
  , intercalate
  , intercalateElem
  , transpose
  -- * Search
  -- ** element-wise search
  , find, findLast
  , filter, partition
  -- * Unicode processing
    -- ** normalization
  , NormalizationResult(..), NormalizeMode(..)
  , isNormalized, isNormalizedTo, normalize, normalizeTo
    -- ** Case conversion
    -- $case
  , Locale, localeDefault, localeLithuanian, localeTurkishAndAzeriLatin
  , caseFold, caseFoldWith, toLower, toLowerWith, toUpper, toUpperWith, toTitle, toTitleWith
    -- ** Unicode category
  , isCategory, spanCategory
  , Category
  , categoryLetterUppercase
  , categoryLetterLowercase
  , categoryLetterTitlecase
  , categoryLetterOther
  , categoryLetter
  , categoryCaseMapped

  , categoryMarkNonSpacing
  , categoryMarkSpacing
  , categoryMarkEnclosing
  , categoryMark

  , categoryNumberDecimal
  , categoryNumberLetter
  , categoryNumberOther
  , categoryNumber

  , categoryPunctuationConnector
  , categoryPunctuationDash
  , categoryPunctuationOpen
  , categoryPunctuationClose
  , categoryPunctuationInitial
  , categoryPunctuationFinal
  , categoryPunctuationOther
  , categoryPunctuation

  , categorySymbolMath
  , categorySymbolCurrency
  , categorySymbolModifier
  , categorySymbolOther
  , categorySymbol

  , categorySeparatorSpace
  , categorySeparatorLine
  , categorySeparatorParagraph
  , categorySeparator
  , categoryControl
  , categoryFormat
  , categorySurrogate
  , categoryPrivateUse
  , categoryUnassigned
  , categoryCompatibility
  , categoryIgnoreGraphemeCluste
  , categoryIscntrl

  , categoryIsprint
  , categoryIsspace
  , categoryIsblank
  , categoryIsgraph
  , categoryIspunct
  , categoryIsalnum
  , categoryIsalpha
  , categoryIsupper
  , categoryIslower
  , categoryIsdigit
  , categoryIsxdigit
 ) where

import           Std.Data.Text.Base
import           Std.Data.Text.Search
import           Std.Data.Text.Extra
import           Prelude                  ()


