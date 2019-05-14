{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : Std.Data.JSON.Base
Description : Fast JSON serialization/deserialization
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

-- This module provides 'Converter' to convert 'Value' to haskell data types, and various tools to help
-- user define 'FromValue', 'ToValue' and 'EncodeJSON' instance.
-}

module Std.Data.JSON.Base
  ( -- * Encode & Decode
    DecodeError
  , decode, decode', decodeChunks, decodeChunks', encodeBytes, encodeText, encodeTextBuilder
  -- * Re-export 'Value' type
  , Value(..)
    -- * parse into JSON Value
  , JV.parseValue, JV.parseValue', JV.parseValueChunks, JV.parseValueChunks'
  -- * Convert 'Value' to Haskell data
  , convert, convert', Converter(..), fail', (<?>), prependContext
  , PathElement(..), ConvertError
  , typeMismatch, fromNull, withBool, withScientific, withBoundedScientific, withRealFloat
  , withBoundedIntegral, withText, withArray, withKeyValues, withFlatMap, withFlatMapR
  , withHashMap, withHashMapR, withEmbeddedJSON
  , (.:), (.:?), (.:!), convertField, convertFieldMaybe, convertFieldMaybe'
  -- * FromValue, ToValue & EncodeJSON
  , defaultSettings, Settings(..)
  , ToValue(..), GToValue(..)
  , FromValue(..), GFromValue(..)
  , EncodeJSON(..), GEncodeJSON(..)
  -- * Helper classes for generics
  , Field, GWriteFields(..), GMergeFields(..), GConstrToValue(..)
  , LookupTable, GFromFields(..), GBuildLookup(..), GConstrFromValue(..)
  , GAddPunctuation(..), GConstrEncodeJSON(..)
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import qualified Control.Monad.Fail           as Fail
import           Control.Monad.ST
import           Data.Data
import           Data.Fixed
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Functor.Sum
import           Data.Hashable
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import           Data.Int
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty           as NonEmpty
import qualified Data.Monoid                  as Monoid
import           Data.Primitive.Types         (Prim)
import qualified Data.Primitive.SmallArray    as A
import           Data.Proxy                   (Proxy (..))
import           Data.Ratio                   (Ratio, (%), numerator, denominator)
import           Data.Scientific              (Scientific, base10Exponent, toBoundedInteger)
import qualified Data.Scientific              as Scientific
import qualified Data.Semigroup               as Semigroup
import           Data.Tagged                  (Tagged (..))
import           Data.Typeable
import           Data.Version                 (Version, parseVersion)
import           Data.Word
import           Data.Word
import           GHC.Exts                     (Proxy#, proxy#)
import           GHC.Generics
import           GHC.Natural
import           GHC.TypeNats
import qualified Std.Data.Builder             as B
import           Std.Data.Generics.Utils
import           Std.Data.JSON.Value          (Value(..))
import qualified Std.Data.JSON.Value          as JV
import qualified Std.Data.JSON.Builder        as JB
import qualified Std.Data.Parser              as P
import qualified Std.Data.Parser.Numeric      as P
import qualified Std.Data.Text                as T
import qualified Std.Data.TextBuilder         as TB
import qualified Std.Data.Vector.Base         as V
import qualified Std.Data.Vector.Extra        as V
import qualified Std.Data.Vector.FlatIntMap   as FIM
import qualified Std.Data.Vector.FlatIntSet   as FIS
import qualified Std.Data.Vector.FlatMap      as FM
import qualified Std.Data.Vector.FlatSet      as FS
import           Text.ParserCombinators.ReadP (readP_to_S)

--------------------------------------------------------------------------------

-- There're two possible failures here:
--
--   * 'P.ParseError' is an error during parsing bytes to 'Value'.
--   * 'ConvertError' is an error when converting 'Value' to target data type.
type DecodeError = Either P.ParseError ConvertError

-- | Decode a JSON doc, only trailing JSON whitespace are allowed.
--
decode' :: FromValue a => V.Bytes -> Either DecodeError a
{-# INLINE decode' #-}
decode' bs = case P.parse_ (JV.value <* JV.skipSpaces <* P.endOfInput) bs of
    Left pErr -> Left (Left pErr)
    Right v -> case convert fromValue v of
        Left cErr -> Left (Right cErr)
        Right r -> Right r

-- | Decode a JSON bytes, return any trailing bytes.
decode :: FromValue a => V.Bytes -> (V.Bytes, Either DecodeError a)
{-# INLINE decode #-}
decode bs = case P.parse JV.value bs of
    (bs', Left pErr) -> (bs', Left (Left pErr))
    (bs', Right v) -> case convert fromValue v of
        Left cErr -> (bs', Left (Right cErr))
        Right r -> (bs', Right r)

-- | Decode JSON doc chunks, return trailing bytes.
decodeChunks :: (FromValue a, Monad m) => m V.Bytes -> V.Bytes -> m (V.Bytes, Either DecodeError a)
{-# INLINE decodeChunks #-}
decodeChunks mb bs = do
    mr <- (P.parseChunks JV.value mb bs)
    case mr of
        (bs', Left pErr) -> pure (bs', Left (Left pErr))
        (bs', Right v) -> case convert fromValue v of
            Left cErr -> pure (bs', Left (Right cErr))
            Right r -> pure (bs', Right r)

-- | Decode JSON doc chunks, consuming trailing JSON whitespaces (other trailing bytes are not allowed).
decodeChunks' :: (FromValue a, Monad m) => m V.Bytes -> V.Bytes -> m (Either DecodeError a)
{-# INLINE decodeChunks' #-}
decodeChunks' mb bs = do
    mr <- (P.parseChunks (JV.value <* JV.skipSpaces <* P.endOfInput) mb bs)
    case mr of
        (_, Left pErr) -> pure (Left (Left pErr))
        (_, Right v) -> case convert fromValue v of
            Left cErr -> pure (Left (Right cErr))
            Right r -> pure (Right r)

-- | Directly encode data to JSON bytes.
encodeBytes :: EncodeJSON a => a -> V.Bytes
{-# INLINE encodeBytes #-}
encodeBytes = B.buildBytes . encodeJSON

-- | Text version 'encodeBytes'.
encodeText :: EncodeJSON a => a -> T.Text
{-# INLINE encodeText #-}
encodeText = TB.buildText . encodeTextBuilder

-- | JSON Docs are guaranteed to be valid UTF-8 texts, so we provide this.
encodeTextBuilder :: EncodeJSON a => a -> TB.TextBuilder ()
{-# INLINE encodeTextBuilder #-}
encodeTextBuilder = TB.unsafeFromBuilder . encodeJSON

-- | Run a 'Converter' with input value.
convert :: (a -> Converter r) -> a -> Either ConvertError r
{-# INLINE convert #-}
convert m v = runConverter (m v) (\ paths msg -> (Left (ConvertError paths msg))) Right

-- | Run a 'Converter' with input value.
convert' :: (FromValue a) => Value -> Either ConvertError a
{-# INLINE convert' #-}
convert' = convert fromValue

--------------------------------------------------------------------------------

-- | Elements of a (JSON) Value path used to describe the location of an error.
data PathElement
    = Key {-# UNPACK #-} !T.Text
        -- ^ Path element of a key into an object,
        -- \"object.key\".
    | Index {-# UNPACK #-} !Int
        -- ^ Path element of an index into an
        -- array, \"array[index]\".
    | Embedded
        -- ^ path of a embedded (JSON) String
  deriving (Eq, Show, Typeable, Ord, Generic, NFData)

data ConvertError = ConvertError { errPath :: [PathElement], errMsg :: T.Text } deriving (Eq, Ord, Generic, NFData)

instance Show ConvertError where
    -- TODO use standard format
    show (ConvertError paths msg) = T.unpack . TB.buildText $ do
        "<"
        mapM_ renderPath (reverse paths)
        "> "
        TB.text msg
      where
        renderPath (Index ix) = TB.char7 '[' >> TB.int ix >> TB.char7 ']'
        renderPath (Key k) = TB.char7 '.' >> (TB.unsafeFromBuilder $ JB.string k)
        renderPath Embedded = "<Embedded>"

-- | 'Converter' for convert result from JSON 'Value'.
--
-- This is intended to be named differently from 'P.Parser' to clear confusions.
newtype Converter a = Converter { runConverter :: forall r. ([PathElement] -> T.Text -> r) -> (a -> r) -> r }

instance Functor Converter where
    fmap f m = Converter (\ kf k -> runConverter m kf (k . f))
    {-# INLINE fmap #-}

instance Applicative Converter where
    pure a = Converter (\ _ k -> k a)
    {-# INLINE pure #-}
    (Converter f) <*> (Converter g) = Converter (\ kf k ->
        f kf (\ f' ->  g kf (k . f')))
    {-# INLINE (<*>) #-}

instance Alternative Converter where
    {-# INLINE (<|>) #-}
    (Converter f) <|> (Converter g) = Converter (\ kf k -> f (\ _ _ -> g kf k) k)
    {-# INLINE empty #-}
    empty = fail' "Std.Data.JSON.Base(Alternative).empty"

instance MonadPlus Converter where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance Monad Converter where
    (Converter f) >>= g = Converter (\ kf k ->
        f kf (\ a -> runConverter (g a) kf k))
    {-# INLINE (>>=) #-}
    return = pure
    {-# INLINE return #-}
    fail = Fail.fail
    {-# INLINE fail #-}

instance Fail.MonadFail Converter where
    {-# INLINE fail #-}
    fail = fail' . T.pack

-- | 'T.Text' version of 'fail'.
fail' :: T.Text -> Converter a
{-# INLINE fail' #-}
fail' msg = Converter (\ kf _ -> kf [] msg)

--------------------------------------------------------------------------------

-- | Produce an error message like @converting XXX failed, expected XXX, encountered XXX@.
typeMismatch :: T.Text     -- ^ The name of the type you are trying to convert.
             -> T.Text     -- ^ The JSON value type you expecting to meet.
             -> Value      -- ^ The actual value encountered.
             -> Converter a
typeMismatch name expected v =
    fail' $ T.concat ["converting ", name, " failed, expected ", expected, ", encountered ", actual]
  where
    actual = case v of
        Object _ -> "Object"
        Array _  -> "Array"
        String _ -> "String"
        Number _ -> "Number"
        Bool _   -> "Boolean"
        Null     -> "Null"

-- | Add JSON Path context to a converter
--
-- When converting a complex structure, it helps to annotate (sub)converters
-- with context, so that if an error occurs, you can find its location.
--
-- > withFlatMapR "Person" $ \o ->
-- >   Person
-- >     <$> o .: "name" <?> Key "name"
-- >     <*> o .: "age" <?> Key "age"
--
-- (Standard methods like '(.:)' already do this.)
--
-- With such annotations, if an error occurs, you will get a JSON Path
-- location of that error.
(<?>) :: Converter a -> PathElement -> Converter a
{-# INLINE (<?>) #-}
(Converter p) <?> path = Converter (\ kf k -> p (kf . (path:)) k)
infixl 9 <?>

-- | Add context to a failure message, indicating the name of the structure
-- being converted.
--
-- > prependContext "MyType" (fail "[error message]")
-- > -- Error: "converting MyType failed, [error message]"
prependContext :: T.Text -> Converter a -> Converter a
{-# INLINE prependContext #-}
prependContext name (Converter p) = Converter (\ kf k ->
    p (\ paths msg -> kf paths (T.concat ["converting ", name, " failed, ", msg])) k)

fromNull :: T.Text -> a -> Value -> Converter a
{-# INLINE fromNull #-}
fromNull _ a Null = pure a
fromNull c _ v    = typeMismatch c "Null" v

withBool :: T.Text -> (Bool -> Converter a) -> Value ->  Converter a
{-# INLINE withBool #-}
withBool _    f (Bool x)  = f x
withBool name f v         = typeMismatch name "Bool" v

-- | @'withScientific' name f value@ applies @f@ to the 'Scientific' number
-- when @value@ is a 'Data.Aeson.Number' and fails using 'typeMismatch'
-- otherwise.
--
-- /Warning/: If you are converting from a scientific to an unbounded
-- type such as 'Integer' you may want to add a restriction on the
-- size of the exponent (see 'withBoundedScientific') to prevent
-- malicious input from filling up the memory of the target system.
--
-- ==== Error message example
--
-- > withScientific "MyType" f (String "oops")
-- > -- Error: "converting MyType failed, expected Number, but encountered String"
withScientific :: T.Text -> (Scientific -> Converter a) -> Value ->  Converter a
{-# INLINE withScientific #-}
withScientific _    f (Number x)  = f x
withScientific name f v           = typeMismatch name "Number" v

-- | @'withRealFloat' try to convert floating number with following rules:
--
--   * Use @±Infinity@ to represent out of range numbers.
--   * Convert @Null@ as @NaN@
--
withRealFloat :: RealFloat a => T.Text -> (a -> Converter r) -> Value -> Converter r
{-# INLINE withRealFloat #-}
withRealFloat _    f (Number s) = f (Scientific.toRealFloat s)
withRealFloat _    f Null       = f (0/0)
withRealFloat name f v          = typeMismatch name "Number or Null" v

-- | @'withBoundedScientific' name f value@ applies @f@ to the 'Scientific' number
-- when @value@ is a 'Number' with exponent less than or equal to 1024.
withBoundedScientific :: T.Text -> (Scientific -> Converter a) -> Value ->  Converter a
{-# INLINE withBoundedScientific #-}
withBoundedScientific name f (Number x)
    | e <= 1024 = f x
    | otherwise = fail' . TB.buildText $ do
        "converting "
        TB.text name
        " failed, found a number with exponent "
        TB.int e
        ", but it must not be greater than 1024"
  where e = base10Exponent x
withBoundedScientific name f v = typeMismatch name "Number" v

-- | @'withBoundedScientific' name f value@ applies @f@ to the 'Scientific' number
-- when @value@ is a 'Number' and value is within @minBound ~ maxBound@.
withBoundedIntegral :: (Bounded a, Integral a) => T.Text -> (a -> Converter r) -> Value -> Converter r
{-# INLINE withBoundedIntegral #-}
withBoundedIntegral name f (Number x) =
    case toBoundedInteger x of
        Just i -> f i
        _      -> fail' . TB.buildText $ do
            "converting "
            TB.text name
            "failed, value is either floating or will cause over or underflow "
            TB.scientific x
withBoundedIntegral name f v = typeMismatch name "Number" v

withText :: T.Text -> (T.Text -> Converter a) -> Value -> Converter a
{-# INLINE withText #-}
withText _    f (String x)  = f x
withText name f v           = typeMismatch name "String" v

withArray :: T.Text -> (V.Vector Value -> Converter a) -> Value -> Converter a
{-# INLINE withArray #-}
withArray _ f (Array arr)  = f arr
withArray name f v         = typeMismatch name "Array" v

-- | Directly use 'Object' as key-values for further converting.
withKeyValues :: T.Text -> (V.Vector (T.Text, Value) -> Converter a) -> Value -> Converter a
{-# INLINE withKeyValues #-}
withKeyValues _    f (Object kvs) = f kvs
withKeyValues name f v            = typeMismatch name "Object" v

-- | Take a 'Object' as an 'FM.FlatMap T.Text Value', on key duplication prefer first one.
withFlatMap :: T.Text -> (FM.FlatMap T.Text Value -> Converter a) -> Value -> Converter a
{-# INLINE withFlatMap #-}
withFlatMap _    f (Object obj) = f (FM.packVector obj)
withFlatMap name f v            = typeMismatch name "Object" v

-- | Take a 'Object' as an 'FM.FlatMap T.Text Value', on key duplication prefer last one.
withFlatMapR :: T.Text -> (FM.FlatMap T.Text Value -> Converter a) -> Value -> Converter a
{-# INLINE withFlatMapR #-}
withFlatMapR _    f (Object obj) = f (FM.packVectorR obj)
withFlatMapR name f v            = typeMismatch name "Object" v

-- | Take a 'Object' as an 'HM.HashMap T.Text Value', on key duplication prefer first one.
withHashMap :: T.Text -> (HM.HashMap T.Text Value -> Converter a) -> Value -> Converter a
{-# INLINE withHashMap #-}
withHashMap _    f (Object obj) = f (HM.fromList (V.unpackR obj))
withHashMap name f v            = typeMismatch name "Object" v

-- | Take a 'Object' as an 'HM.HashMap T.Text Value', on key duplication prefer last one.
withHashMapR :: T.Text -> (HM.HashMap T.Text Value -> Converter a) -> Value -> Converter a
{-# INLINE withHashMapR #-}
withHashMapR _    f (Object obj) = f (HM.fromList (V.unpack obj))
withHashMapR name f v            = typeMismatch name "Object" v

-- | Decode a nested JSON-encoded string.
withEmbeddedJSON :: T.Text                  -- ^ data type name
                 -> (Value -> Converter a)     -- ^ a inner converter which will get the converted 'Value'.
                 -> Value -> Converter a       -- a converter take a JSON String
{-# INLINE withEmbeddedJSON #-}
withEmbeddedJSON name innerConverter (String txt) = Converter (\ kf k ->
        case decode' (T.getUTF8Bytes txt) of
            Right v -> runConverter (innerConverter v) (\ paths msg -> kf (Embedded:paths) msg) k
            Left (Left pErr) -> kf [] (T.intercalate ", " ("parsing embeded JSON failed ": pErr))
            _                -> error "Std.JSON.Base: impossible, converting to Value should not fail")
withEmbeddedJSON name _ v = typeMismatch name "String" v

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '.:?' instead.
(.:) :: (FromValue a) => FM.FlatMap T.Text Value -> T.Text -> Converter a
{-# INLINE (.:) #-}
(.:) = convertField fromValue

-- | Retrieve the value associated with the given key of an 'Object'. The
-- result is 'Nothing' if the key is not present or if its value is 'Null',
-- or 'empty' if the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
(.:?) :: (FromValue a) => FM.FlatMap T.Text Value -> T.Text -> Converter (Maybe a)
{-# INLINE (.:?) #-}
(.:?) = convertFieldMaybe fromValue

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present or 'empty' if the
-- value cannot be converted to the desired type.
--
-- This differs from '.:?' by attempting to convert 'Null' the same as any
-- other JSON value, instead of interpreting it as 'Nothing'.
(.:!) :: (FromValue a) => FM.FlatMap T.Text Value -> T.Text -> Converter (Maybe a)
{-# INLINE (.:!) #-}
(.:!) = convertFieldMaybe' fromValue

convertField :: (Value -> Converter a)  -- ^ the field converter (value part of a key value pair)
           -> FM.FlatMap T.Text Value -> T.Text -> Converter a
{-# INLINE convertField #-}
convertField p obj key = case FM.lookup key obj of
    Just v -> p v <?> Key key
    _      -> fail' (T.concat $ ["key ", key, " not present"])

-- | Variant of '.:?' with explicit converter function.
convertFieldMaybe :: (Value -> Converter a) -> FM.FlatMap T.Text Value -> T.Text -> Converter (Maybe a)
{-# INLINE convertFieldMaybe #-}
convertFieldMaybe p obj key = case FM.lookup key obj of
    Just Null -> pure Nothing
    Just v    -> Just <$> p v <?> Key key
    _         -> pure Nothing

-- | Variant of '.:!' with explicit converter function.
convertFieldMaybe' :: (Value -> Converter a) -> FM.FlatMap T.Text Value -> T.Text -> Converter (Maybe a)
{-# INLINE convertFieldMaybe' #-}
convertFieldMaybe' p obj key = case FM.lookup key obj of
    Just v  -> Just <$> p v <?> Key key
    _       -> pure Nothing

--------------------------------------------------------------------------------

-- | Use @,@ as separator to connect list of builders.
commaList' :: EncodeJSON a => [a] -> B.Builder ()
{-# INLINE commaList' #-}
commaList' = B.intercalateList B.comma encodeJSON

-- | Use @,@ as separator to connect a vector of builders.
commaVec' :: (EncodeJSON a, V.Vec v a) => v a ->  B.Builder ()
{-# INLINE commaVec' #-}
commaVec' = B.intercalateVec B.comma encodeJSON

--------------------------------------------------------------------------------

-- | Generic encode/decode Settings
--
-- There should be no control charactors in formatted texts since we don't escaping those
-- field names or constructor names ('defaultSettings' relys on Haskell's lexical property).
-- Otherwise 'encodeJSON' will output illegal JSON string.
data Settings = Settings
    { fieldFmt :: String -> T.Text  -- ^ format field labels
    , constrFmt :: String -> T.Text -- ^ format constructor names.
    }

defaultSettings :: Settings
defaultSettings = Settings T.pack T.pack

--------------------------------------------------------------------------------
-- ToValue
--------------------------------------------------------------------------------

-- | Typeclass for converting to JSON 'Value'.
class ToValue a where
    toValue :: a -> Value
    default toValue :: (Generic a, GToValue (Rep a)) => a -> Value
    toValue = gToValue defaultSettings . from

class GToValue f where
    gToValue :: Settings -> f a -> Value

--------------------------------------------------------------------------------
-- Selectors

type family Field f where
    Field (a :*: b) = Field a
    Field (S1 (MetaSel Nothing u ss ds) f) = Value
    Field (S1 (MetaSel (Just l) u ss ds) f) = (T.Text, Value)

class GWriteFields f where
    gWriteFields :: Settings -> A.SmallMutableArray s (Field f) -> Int -> f a -> ST s ()

instance (ProductSize a, GWriteFields a, GWriteFields b, Field a ~ Field b) => GWriteFields (a :*: b) where
    {-# INLINE gWriteFields #-}
    gWriteFields s marr idx (a :*: b) = do
        gWriteFields s marr idx a
        gWriteFields s marr (idx + productSize (proxy# :: Proxy# a)) b

instance (GToValue f) => GWriteFields (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gWriteFields #-}
    gWriteFields s marr idx (M1 x) = A.writeSmallArray marr idx (gToValue s x)

instance (GToValue f, Selector (MetaSel (Just l) u ss ds)) => GWriteFields (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gWriteFields #-}
    gWriteFields s marr idx m1@(M1 x) = A.writeSmallArray marr idx ((fieldFmt s) (selName m1), gToValue s x)

instance (GToValue f, Selector (MetaSel (Just l) u ss ds)) => GToValue (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gToValue #-}
    gToValue s m1@(M1 x) =
        let k = fieldFmt s $ selName m1
            v = gToValue s x
        in Object (V.singleton (k, v))

instance GToValue f => GToValue (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gToValue #-}
    gToValue s (M1 x) = gToValue s x

instance ToValue a => GToValue (K1 i a) where
    {-# INLINE gToValue #-}
    gToValue s (K1 x) = toValue x

class GMergeFields f where
    gMergeFields :: Proxy# f -> A.SmallMutableArray s (Field f) -> ST s Value

instance GMergeFields a => GMergeFields (a :*: b) where
    {-# INLINE gMergeFields #-}
    gMergeFields _ = gMergeFields (proxy# :: Proxy# a)

instance GMergeFields (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gMergeFields #-}
    gMergeFields _ marr = do
        arr <- A.unsafeFreezeSmallArray marr
        let l = A.sizeofSmallArray arr
        pure (Array (V.Vector arr 0 l))

instance GMergeFields (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gMergeFields #-}
    gMergeFields _ marr = do
        arr <- A.unsafeFreezeSmallArray marr
        let l = A.sizeofSmallArray arr
        pure (Object (V.Vector arr 0 l))

--------------------------------------------------------------------------------
-- Constructors

class GConstrToValue f where
    gConstrToValue :: Bool -> Settings -> f a -> Value

instance GConstrToValue V1 where
    {-# INLINE gConstrToValue #-}
    gConstrToValue _ _ _ = error "Std.Data.JSON.Base: empty data type"

instance (GConstrToValue f, GConstrToValue g) => GConstrToValue (f :+: g) where
    {-# INLINE gConstrToValue #-}
    gConstrToValue _ s (L1 x) = gConstrToValue True s x
    gConstrToValue _ s (R1 x) = gConstrToValue True s x

-- | Constructor without payload, convert to String
instance (Constructor c) => GConstrToValue (C1 c U1) where
    {-# INLINE gConstrToValue #-}
    gConstrToValue _ s (M1 _) = String . constrFmt s $ conName (undefined :: t c U1 a)

-- | Constructor with a single payload
instance (Constructor c, GToValue (S1 sc f)) => GConstrToValue (C1 c (S1 sc f)) where
    {-# INLINE gConstrToValue #-}
    gConstrToValue False s (M1 x) = gToValue s x
    gConstrToValue True s (M1 x) =
        let k = constrFmt s $ conName (undefined :: t c f a)
            v = gToValue s x
        in Object (V.singleton (k, v))

-- | Constructor with multiple payloads
instance (ProductSize (a :*: b), GWriteFields (a :*: b), GMergeFields (a :*: b), Constructor c)
    => GConstrToValue (C1 c (a :*: b)) where
    {-# INLINE gConstrToValue #-}
    gConstrToValue False s (M1 x) = runST (do
        marr <- A.newSmallArray (productSize (proxy# :: Proxy# (a :*: b))) undefined
        gWriteFields s marr 0 x
        gMergeFields (proxy# :: Proxy# (a :*: b)) marr)
    gConstrToValue True s (M1 x) =
        let k = constrFmt s $ conName (undefined :: t c f a)
            v = runST (do
                    marr <- A.newSmallArray (productSize (proxy# :: Proxy# (a :*: b))) undefined
                    gWriteFields s marr 0 x
                    gMergeFields (proxy# :: Proxy# (a :*: b)) marr)
        in Object (V.singleton (k, v))

--------------------------------------------------------------------------------
-- Data types
instance GConstrToValue f => GToValue (D1 c f) where
    {-# INLINE gToValue #-}
    gToValue s (M1 x) = gConstrToValue False s x

--------------------------------------------------------------------------------
-- EncodeJSON
--------------------------------------------------------------------------------

class EncodeJSON a where
    encodeJSON :: a -> B.Builder ()
    default encodeJSON :: (Generic a, GEncodeJSON (Rep a)) => a -> B.Builder ()
    encodeJSON = gEncodeJSON defaultSettings . from

encodeJSONText :: EncodeJSON a => a -> TB.TextBuilder ()
{-# INLINE encodeJSONText #-}
encodeJSONText = TB.unsafeFromBuilder . encodeJSON

class GEncodeJSON f where
    gEncodeJSON :: Settings -> f a -> B.Builder ()

--------------------------------------------------------------------------------
-- Selectors

instance (GEncodeJSON f, Selector (MetaSel (Just l) u ss ds)) => GEncodeJSON (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s m1@(M1 x) = (fieldFmt s $ selName m1) `JB.kv` gEncodeJSON s x

instance GEncodeJSON f => GEncodeJSON (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s (M1 x) = gEncodeJSON s x

instance (GEncodeJSON a, GEncodeJSON b) => GEncodeJSON (a :*: b) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s (a :*: b) = gEncodeJSON s a >> B.comma >> gEncodeJSON s b

instance EncodeJSON a => GEncodeJSON (K1 i a) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s (K1 x) = encodeJSON x

class GAddPunctuation (f :: * -> *) where
    gAddPunctuation :: Proxy# f -> B.Builder () -> B.Builder ()

instance GAddPunctuation a => GAddPunctuation (a :*: b) where
    {-# INLINE gAddPunctuation #-}
    gAddPunctuation _ = gAddPunctuation (proxy# :: Proxy# a)

instance GAddPunctuation (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gAddPunctuation #-}
    gAddPunctuation _ b = B.square b

instance GAddPunctuation (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gAddPunctuation #-}
    gAddPunctuation _ b = B.curly b

--------------------------------------------------------------------------------
-- Constructors

class GConstrEncodeJSON f where
    gConstrEncodeJSON :: Bool -> Settings -> f a -> B.Builder ()

instance GConstrEncodeJSON V1 where
    {-# INLINE gConstrEncodeJSON #-}
    gConstrEncodeJSON _ _ _ = error "Std.Data.JSON.Base: empty data type"

instance (GConstrEncodeJSON f, GConstrEncodeJSON g) => GConstrEncodeJSON (f :+: g) where
    {-# INLINE gConstrEncodeJSON #-}
    gConstrEncodeJSON _ s (L1 x) = gConstrEncodeJSON True s x
    gConstrEncodeJSON _ s (R1 x) = gConstrEncodeJSON True s x

-- | Constructor without payload, convert to String
instance (Constructor c) => GConstrEncodeJSON (C1 c U1) where
    {-# INLINE gConstrEncodeJSON #-}
    -- There should be no chars need escaping in constructor name
    gConstrEncodeJSON _ s (M1 _) = B.quotes $
        B.text . constrFmt s $ conName (undefined :: t c U1 a)

-- | Constructor with a single payload
instance (Constructor c, GEncodeJSON (S1 sc f)) => GConstrEncodeJSON (C1 c (S1 sc f)) where
    {-# INLINE gConstrEncodeJSON #-}
    gConstrEncodeJSON False s (M1 x) = gEncodeJSON s x
    gConstrEncodeJSON True s (M1 x) = B.curly $ do
        (constrFmt s $ conName (undefined :: t c f a)) `JB.kv` gEncodeJSON s x

-- | Constructor with multiple payloads
instance (GEncodeJSON (a :*: b), GAddPunctuation (a :*: b), Constructor c)
    => GConstrEncodeJSON (C1 c (a :*: b)) where
    {-# INLINE gConstrEncodeJSON #-}
    gConstrEncodeJSON False s (M1 x) = gAddPunctuation (proxy# :: Proxy# (a :*: b)) (gEncodeJSON s x)
    gConstrEncodeJSON True s (M1 x) = B.curly $ do
        (constrFmt s $ conName (undefined :: t c f a)) `JB.kv`
            gAddPunctuation (proxy# :: Proxy# (a :*: b)) (gEncodeJSON s x)

--------------------------------------------------------------------------------
-- Data types
instance GConstrEncodeJSON f => GEncodeJSON (D1 c f) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s (M1 x) = gConstrEncodeJSON False s x

--------------------------------------------------------------------------------
-- FromValue
--------------------------------------------------------------------------------

class FromValue a where
    fromValue :: Value -> Converter a
    default fromValue :: (Generic a, GFromValue (Rep a)) => Value -> Converter a
    fromValue v = to <$> gFromValue defaultSettings v

class GFromValue f where
    gFromValue :: Settings -> Value -> Converter (f a)

--------------------------------------------------------------------------------
-- Selectors

type family LookupTable f where
    LookupTable (a :*: b) = LookupTable a
    LookupTable (S1 (MetaSel Nothing u ss ds) f) = V.Vector Value
    LookupTable (S1 (MetaSel (Just l) u ss ds) f) = FM.FlatMap T.Text Value

class GFromFields f where
    gFromFields :: Settings -> LookupTable f -> Int -> Converter (f a)

instance (ProductSize a, GFromFields a, GFromFields b, LookupTable a ~ LookupTable b)
    => GFromFields (a :*: b) where
    {-# INLINE gFromFields #-}
    gFromFields s v idx = do
        a <- gFromFields s v idx
        b <- gFromFields s v (idx + productSize (proxy# :: Proxy# a))
        pure (a :*: b)

instance (GFromValue f) => GFromFields (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFromFields #-}
    gFromFields s v idx = do
        v' <- V.unsafeIndexM v idx
        M1 <$> gFromValue s v' <?> Index idx

instance (GFromValue f, Selector (MetaSel (Just l) u ss ds)) => GFromFields (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gFromFields #-}
    gFromFields s v _ = do
        case FM.lookup fn v of
            Just v' -> M1 <$> gFromValue s v' <?> Key fn
            _       -> fail' ("Std.Data.JSON.Base: missing field " <>  fn)
      where
        fn = (fieldFmt s) (selName (undefined :: S1 (MetaSel (Just l) u ss ds) f a))

instance GFromValue f => GFromValue (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFromValue #-}
    gFromValue s x = M1 <$> gFromValue s x

instance (GFromValue f, Selector (MetaSel (Just l) u ss ds)) => GFromValue (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gFromValue #-}
    gFromValue s (Object v) = do
        case FM.lookup fn (FM.packVectorR v) of
            Just v' -> M1 <$> gFromValue s v' <?> Key fn
            _       -> fail' ("Std.Data.JSON.Base: missing field " <>  fn)
      where fn = (fieldFmt s) (selName (undefined :: S1 (MetaSel (Just l) u ss ds) f a))
    gFromValue s v = typeMismatch ("field " <> fn) "Object" v <?> Key fn
      where fn = (fieldFmt s) (selName (undefined :: S1 (MetaSel (Just l) u ss ds) f a))

instance FromValue a => GFromValue (K1 i a) where
    {-# INLINE gFromValue #-}
    gFromValue s x = K1 <$> fromValue x

class GBuildLookup f where
    gBuildLookup :: Proxy# f -> Int -> T.Text -> Value -> Converter (LookupTable f)

instance (GBuildLookup a, GBuildLookup b) => GBuildLookup (a :*: b) where
    {-# INLINE gBuildLookup #-}
    gBuildLookup _ siz = gBuildLookup (proxy# :: Proxy# a) siz

instance GBuildLookup (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gBuildLookup #-}
    gBuildLookup _ siz name (Array v)
        | siz' /= siz = fail' . TB.buildText $ do
            "converting "
            TB.text name
            " failed, product size mismatch, expected "
            TB.int siz
            ", get"
            TB.int siz'
        | otherwise = pure v
      where siz' = V.length v
    gBuildLookup _ _   name x         = typeMismatch name "Array" x

instance GBuildLookup (S1 ((MetaSel (Just l) u ss ds)) f) where
    {-# INLINE gBuildLookup #-}
    gBuildLookup _ siz name (Object v)
        | siz' /= siz = fail' . TB.buildText $ do
            "converting "
            TB.text name
            " failed, product size mismatch, expected "
            TB.int siz
            ", get"
            TB.int siz'
        | otherwise = pure m
      where siz' = FM.size m
            m = FM.packVectorR v
    gBuildLookup _ _   name x       = typeMismatch name "Object" x

--------------------------------------------------------------------------------
-- Constructors

class GConstrFromValue f where
    gConstrFromValue :: Bool -> Settings -> Value -> Converter (f a)

instance GConstrFromValue V1 where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue _ _ _ = error "Std.Data.JSON.Base: empty data type"

instance (GConstrFromValue f, GConstrFromValue g) => GConstrFromValue (f :+: g) where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue _ s x = (L1 <$> gConstrFromValue True s x) <|> (R1 <$> gConstrFromValue True s x)

-- | Constructor without payload, convert to String
instance (Constructor c) => GConstrFromValue (C1 c U1) where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue _ s (String x)
        | cn == x   = pure (M1 U1)
        | otherwise = fail' . T.concat $ ["converting ", cn', "failed, unknown constructor name ", x]
      where cn = constrFmt s $ conName (undefined :: t c U1 a)
            cn' = T.pack $ conName (undefined :: t c U1 a)
    gConstrFromValue _ _ v = typeMismatch cn' "String" v
      where cn' = T.pack $ conName (undefined :: t c U1 a)

-- | Constructor with a single payload
instance (Constructor c, GFromValue (S1 sc f)) => GConstrFromValue (C1 c (S1 sc f)) where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue False s x = M1 <$> gFromValue s x
    gConstrFromValue True s x = case x of
        Object v -> case V.indexM v 0 of
            Just (k, v') | k == cn -> M1 <$> gFromValue s v' <?> Key cn
            _                      -> fail' .T.concat $ ["converting ", cn', " failed, constructor not found"]
        _ ->  typeMismatch cn' "Object" x
      where cn = constrFmt s $ conName (undefined :: t c f a)
            cn' = T.pack $ conName (undefined :: t c f a)

-- | Constructor with multiple payloads
instance (ProductSize (a :*: b), GFromFields (a :*: b), GBuildLookup (a :*: b), Constructor c)
    => GConstrFromValue (C1 c (a :*: b)) where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue False s x = do
        t <- gBuildLookup p (productSize p) cn' x
        M1 <$> gFromFields s t 0
      where cn' = T.pack $ conName (undefined :: t c f a)
            p = proxy# :: Proxy# (a :*: b)
    gConstrFromValue True s x = case x of
        Object v -> case V.indexM v 0 of
            Just (k, v') | k == cn -> do t <- gBuildLookup p (productSize p) cn' v'
                                         M1 <$> gFromFields s t 0
            _                      -> fail' .T.concat $ ["converting ", cn', " failed, constructor not found"]
        _ ->  typeMismatch cn' "Object" x
      where cn = constrFmt s $ conName (undefined :: t c f a)
            cn' = T.pack $ conName (undefined :: t c f a)
            p = proxy# :: Proxy# (a :*: b)

--------------------------------------------------------------------------------
-- Data types
instance GConstrFromValue f => GFromValue (D1 c f) where
    {-# INLINE gFromValue #-}
    gFromValue s x = M1 <$> gConstrFromValue False s x

--------------------------------------------------------------------------------
-- Built-in Instances
--------------------------------------------------------------------------------
-- | Use 'Null' as @Proxy a@
instance FromValue (Proxy a)   where {{-# INLINE fromValue #-}; fromValue = fromNull "Proxy" Proxy;}
instance ToValue (Proxy a)     where {{-# INLINE toValue #-}; toValue _ = Null;}
instance EncodeJSON (Proxy a) where {{-# INLINE encodeJSON #-}; encodeJSON _ = "null";}

instance FromValue Value   where {{-# INLINE fromValue #-}; fromValue = pure;}
instance ToValue Value     where { {-# INLINE toValue #-}; toValue = id; }
instance EncodeJSON Value where { {-# INLINE encodeJSON #-}; encodeJSON = JB.value; }

instance FromValue T.Text   where {{-# INLINE fromValue #-}; fromValue = withText "Text" pure;}
instance ToValue T.Text     where {{-# INLINE toValue #-}; toValue = String;}
instance EncodeJSON T.Text where {{-# INLINE encodeJSON #-}; encodeJSON = JB.string;}

instance FromValue TB.Str where
    {-# INLINE fromValue #-}
    fromValue = withText "Str" (pure . TB.Str . T.unpack)
instance ToValue TB.Str where
    {-# INLINE toValue #-}
    toValue = String . T.pack . TB.chrs
instance EncodeJSON TB.Str where
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.string . T.pack . TB.chrs

instance FromValue Scientific where {{-# INLINE fromValue #-}; fromValue = withScientific "Scientific" pure;}
instance ToValue Scientific where {{-# INLINE toValue #-}; toValue = Number;}
instance EncodeJSON Scientific where {{-# INLINE encodeJSON #-}; encodeJSON = B.scientific;}

-- | default instance prefer later key
instance FromValue a => FromValue (FM.FlatMap T.Text a) where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "Std.Data.Vector.FlatMap.FlatMap"
        (FM.traverseWithKey $ \ k v -> fromValue v <?> Key k)
instance ToValue a => ToValue (FM.FlatMap T.Text a) where
    {-# INLINE toValue #-}
    toValue = Object . FM.sortedKeyValues . FM.map' toValue
instance EncodeJSON a => EncodeJSON (FM.FlatMap T.Text a) where
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.object' encodeJSON . FM.sortedKeyValues

instance (Ord a, FromValue a) => FromValue (FS.FlatSet a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Std.Data.Vector.FlatSet.FlatSet" $ \ v ->
        FS.packRN (V.length v) <$>
            (zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack v))
instance ToValue a => ToValue (FS.FlatSet a) where
    {-# INLINE toValue #-}
    toValue = Array . V.map' toValue . FS.sortedValues
instance EncodeJSON a => EncodeJSON (FS.FlatSet a) where
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.array' encodeJSON . FS.sortedValues

-- | default instance prefer later key
instance FromValue a => FromValue (HM.HashMap T.Text a) where
    {-# INLINE fromValue #-}
    fromValue = withHashMapR "Data.HashMap.HashMap"
        (HM.traverseWithKey $ \ k v -> fromValue v <?> Key k)
instance ToValue a => ToValue (HM.HashMap T.Text a) where
    {-# INLINE toValue #-}
    toValue = Object . V.pack . HM.toList . HM.map toValue
instance EncodeJSON a => EncodeJSON (HM.HashMap T.Text a) where
    {-# INLINE encodeJSON #-}
    encodeJSON = B.curly . B.intercalateList B.comma (\ (k, v) -> k `JB.kv'` encodeJSON v) . HM.toList

instance FromValue a => FromValue (FIM.FlatIntMap a) where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "Std.Data.Vector.FlatIntMap.FlatIntMap" $ \ m ->
        let kvs = FM.sortedKeyValues m
        in FIM.packVectorR <$> (forM kvs $ \ (k, v) -> do
            case P.parse_ P.int (T.getUTF8Bytes k) of
                Right k' -> do
                    v' <- fromValue v <?> Key k
                    return (V.IPair k' v')
                _ -> fail' ("converting Std.Data.Vector.FlatIntMap.FlatIntMap failed, unexpected key " <> k))
instance ToValue a => ToValue (FIM.FlatIntMap a) where
    {-# INLINE toValue #-}
    toValue = Object . V.map' toKV . FIM.sortedKeyValues
      where toKV (V.IPair i x) = let !k = TB.buildText (TB.int i)
                                     !v = toValue x
                                 in (k, v)
instance EncodeJSON a => EncodeJSON (FIM.FlatIntMap a) where
    {-# INLINE encodeJSON #-}
    encodeJSON = B.curly . B.intercalateVec B.comma (\ (V.IPair i x) -> do
        B.quotes (B.int i)
        B.colon
        encodeJSON x) . FIM.sortedKeyValues

instance FromValue FIS.FlatIntSet where
    {-# INLINE fromValue #-}
    fromValue = withArray "Std.Data.Vector.FlatIntSet.FlatIntSet" $ \ v ->
        FIS.packRN (V.length v) <$> zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack v)
instance ToValue FIS.FlatIntSet where
    {-# INLINE toValue #-}
    toValue = toValue . FIS.sortedValues
instance EncodeJSON FIS.FlatIntSet where
    {-# INLINE encodeJSON #-}
    encodeJSON = encodeJSON . FIS.sortedValues

instance FromValue a => FromValue (V.Vector a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Std.Data.Vector.Vector"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
instance ToValue a => ToValue (V.Vector a) where
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
instance EncodeJSON a => EncodeJSON (V.Vector a) where
    {-# INLINE encodeJSON #-}
    encodeJSON = B.square . commaVec'

instance (Prim a, FromValue a) => FromValue (V.PrimVector a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Std.Data.Vector.PrimVector"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
instance (Prim a, ToValue a) => ToValue (V.PrimVector a) where
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
instance (Prim a, EncodeJSON a) => EncodeJSON (V.PrimVector a) where
    {-# INLINE encodeJSON #-}
    encodeJSON = B.square . commaVec'

instance (Eq a, Hashable a, FromValue a) => FromValue (HS.HashSet a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Std.Data.Vector.FlatSet.FlatSet" $ \ v ->
        HS.fromList <$>
            (zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack v))
instance (ToValue a) => ToValue (HS.HashSet a) where
    {-# INLINE toValue #-}
    toValue = toValue . HS.toList
instance (EncodeJSON a) => EncodeJSON (HS.HashSet a) where
    {-# INLINE encodeJSON #-}
    encodeJSON = encodeJSON . HS.toList

instance FromValue a => FromValue [a] where
    {-# INLINE fromValue #-}
    fromValue = withArray "[a]" $ \ v ->
        zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack v)
instance ToValue a => ToValue [a] where
    {-# INLINE toValue #-}
    toValue = Array . V.pack . map toValue
instance EncodeJSON a => EncodeJSON [a] where
    {-# INLINE encodeJSON #-}
    encodeJSON = B.square . commaList'

instance FromValue a => FromValue (NonEmpty a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "NonEmpty" $ \ v -> do
        l <- zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack v)
        case l of (x:xs) -> pure (x :| xs)
                  _      -> fail' "unexpected empty array"
instance (ToValue a) => ToValue (NonEmpty a) where
    {-# INLINE toValue #-}
    toValue = toValue . NonEmpty.toList
instance (EncodeJSON a) => EncodeJSON (NonEmpty a) where
    {-# INLINE encodeJSON #-}
    encodeJSON = encodeJSON . NonEmpty.toList

instance FromValue Bool where {{-# INLINE fromValue #-}; fromValue = withBool "Bool" pure;}
instance ToValue Bool where {{-# INLINE toValue #-}; toValue = Bool; }
instance EncodeJSON Bool where {{-# INLINE encodeJSON #-}; encodeJSON True = "true"; encodeJSON _ = "false";}

instance FromValue Char where
    {-# INLINE fromValue #-}
    fromValue = withText "Char" $ \ t ->
        case T.headMaybe t of
            Just c -> pure c
            _      -> fail' (T.concat ["converting Char failed, expected a string of length 1"])
instance ToValue Char where
    {-# INLINE toValue #-}
    toValue = String . T.singleton
instance EncodeJSON Char where
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.string . T.singleton


instance FromValue Double where {{-# INLINE fromValue #-}; fromValue = withRealFloat "Double" pure;}
instance FromValue Float  where {{-# INLINE fromValue #-}; fromValue = withRealFloat "Double" pure;}
instance ToValue Float  where {{-# INLINE toValue #-}; toValue = Number . P.floatToScientific;}
instance ToValue Double where {{-# INLINE toValue #-}; toValue = Number . P.doubleToScientific;}
instance EncodeJSON Float  where {{-# INLINE encodeJSON #-}; encodeJSON = B.float;}
instance EncodeJSON Double where {{-# INLINE encodeJSON #-}; encodeJSON = B.double;}

instance FromValue Int    where {{-# INLINE fromValue #-}; fromValue = withBoundedIntegral "Int" pure;}
instance FromValue Int8   where {{-# INLINE fromValue #-}; fromValue = withBoundedIntegral "Int8" pure;}
instance FromValue Int16  where {{-# INLINE fromValue #-}; fromValue = withBoundedIntegral "Int16" pure;}
instance FromValue Int32  where {{-# INLINE fromValue #-}; fromValue = withBoundedIntegral "Int32" pure;}
instance FromValue Int64  where {{-# INLINE fromValue #-}; fromValue = withBoundedIntegral "Int64" pure;}
instance FromValue Word   where {{-# INLINE fromValue #-}; fromValue = withBoundedIntegral "Word" pure;}
instance FromValue Word8  where {{-# INLINE fromValue #-}; fromValue = withBoundedIntegral "Word8" pure;}
instance FromValue Word16 where {{-# INLINE fromValue #-}; fromValue = withBoundedIntegral "Word16" pure;}
instance FromValue Word32 where {{-# INLINE fromValue #-}; fromValue = withBoundedIntegral "Word32" pure;}
instance FromValue Word64 where {{-# INLINE fromValue #-}; fromValue = withBoundedIntegral "Word64" pure;}
instance ToValue Int    where {{-# INLINE toValue #-}; toValue = Number . fromIntegral;}
instance ToValue Int8   where {{-# INLINE toValue #-}; toValue = Number . fromIntegral;}
instance ToValue Int16  where {{-# INLINE toValue #-}; toValue = Number . fromIntegral;}
instance ToValue Int32  where {{-# INLINE toValue #-}; toValue = Number . fromIntegral;}
instance ToValue Int64  where {{-# INLINE toValue #-}; toValue = Number . fromIntegral;}
instance ToValue Word   where {{-# INLINE toValue #-}; toValue = Number . fromIntegral;}
instance ToValue Word8  where {{-# INLINE toValue #-}; toValue = Number . fromIntegral;}
instance ToValue Word16 where {{-# INLINE toValue #-}; toValue = Number . fromIntegral;}
instance ToValue Word32 where {{-# INLINE toValue #-}; toValue = Number . fromIntegral;}
instance ToValue Word64 where {{-# INLINE toValue #-}; toValue = Number . fromIntegral;}
instance EncodeJSON Int   where {{-# INLINE encodeJSON #-}; encodeJSON = B.int;}
instance EncodeJSON Int8  where {{-# INLINE encodeJSON #-}; encodeJSON = B.int;}
instance EncodeJSON Int16 where {{-# INLINE encodeJSON #-}; encodeJSON = B.int;}
instance EncodeJSON Int32 where {{-# INLINE encodeJSON #-}; encodeJSON = B.int;}
instance EncodeJSON Int64 where {{-# INLINE encodeJSON #-}; encodeJSON = B.int;}
instance EncodeJSON Word   where {{-# INLINE encodeJSON #-}; encodeJSON = B.int;}
instance EncodeJSON Word8  where {{-# INLINE encodeJSON #-}; encodeJSON = B.int;}
instance EncodeJSON Word16 where {{-# INLINE encodeJSON #-}; encodeJSON = B.int;}
instance EncodeJSON Word32 where {{-# INLINE encodeJSON #-}; encodeJSON = B.int;}
instance EncodeJSON Word64 where {{-# INLINE encodeJSON #-}; encodeJSON = B.int;}

-- | This instance includes a bounds check to prevent maliciously large inputs to fill up the memory of the target system. You can newtype Scientific and provide your own instance using 'withScientific' if you want to allow larger inputs.
instance FromValue Integer where
    {-# INLINE fromValue #-}
    fromValue = withBoundedScientific "Integer" $ \ n ->
        case Scientific.floatingOrInteger n :: Either Double Integer of
            Right x -> pure x
            Left _  -> fail' . TB.buildText $ do
                "converting Integer failed, unexpected floating number "
                TB.scientific n
instance ToValue Integer where
    {-# INLINE toValue #-}
    toValue = Number . fromIntegral
instance EncodeJSON Integer where
    {-# INLINE encodeJSON #-}
    encodeJSON = B.integer

instance FromValue Natural where
    {-# INLINE fromValue #-}
    fromValue = withBoundedScientific "Natural" $ \ n ->
        if n < 0
        then fail' . TB.buildText $ do
                "converting Natural failed, unexpected negative number "
                TB.scientific n
        else case Scientific.floatingOrInteger n :: Either Double Natural of
            Right x -> pure x
            Left _  -> fail' . TB.buildText $ do
                "converting Natural failed, unexpected floating number "
                TB.scientific n
instance ToValue Natural where
    {-# INLINE toValue #-}
    toValue = Number . fromIntegral
instance EncodeJSON Natural where
    {-# INLINE encodeJSON #-}
    encodeJSON = B.integer . fromIntegral

instance FromValue Ordering where
    fromValue = withText "Ordering" $ \ s ->
        case s of
            "LT" -> pure LT
            "EQ" -> pure EQ
            "GT" -> pure GT
            _ -> fail' . T.concat $ ["converting Ordering failed, unexpected ",
                                        s, " expected \"LT\", \"EQ\", or \"GT\""]
instance ToValue Ordering where
    {-# INLINE toValue #-}
    toValue LT = String "LT"
    toValue EQ = String "EQ"
    toValue GT = String "GT"
instance EncodeJSON Ordering where
    {-# INLINE encodeJSON #-}
    encodeJSON LT = "LT"
    encodeJSON EQ = "EQ"
    encodeJSON GT = "GT"

instance FromValue () where
    {-# INLINE fromValue #-}
    fromValue = withArray "()" $ \ v ->
        if V.null v
        then pure ()
        else fail' "converting () failed, expected an empty array"
instance ToValue () where
    {-# INLINE toValue #-}
    toValue () = Array V.empty
instance EncodeJSON () where
    {-# INLINE encodeJSON #-}
    encodeJSON () = "[]"

instance FromValue Version where
    {-# INLINE fromValue #-}
    fromValue = withText "Version" (go . readP_to_S parseVersion . T.unpack)
      where
        go [(v,[])] = pure v
        go (_ : xs) = go xs
        go _        = fail "converting Version failed"
instance ToValue Version where
    {-# INLINE toValue #-}
    toValue = String . T.pack . show
instance EncodeJSON Version where
    {-# INLINE encodeJSON #-}
    encodeJSON = B.string7 . show

instance FromValue a => FromValue (Maybe a) where
    {-# INLINE fromValue #-}
    fromValue Null = pure Nothing
    fromValue v = Just <$> fromValue v
instance ToValue a => ToValue (Maybe a) where
    {-# INLINE toValue #-}
    toValue Nothing = Null
    toValue (Just x) = toValue x
instance EncodeJSON a => EncodeJSON (Maybe a) where
    {-# INLINE encodeJSON #-}
    encodeJSON Nothing = "null"
    encodeJSON (Just x) = encodeJSON x

-- | This instance includes a bounds check to prevent maliciously large inputs to fill up the memory of the target system. You can newtype Ratio and provide your own instance using 'withScientific' if you want to allow larger inputs.
instance (FromValue a, Integral a) => FromValue (Ratio a) where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "Rational" $ \obj -> do
        numerator <- obj .: "numerator"
        denominator <- obj .: "denominator"
        if denominator == 0
        then fail' "Ratio denominator was 0"
        else pure (numerator % denominator)
instance (ToValue a, Integral a) => ToValue (Ratio a) where
    {-# INLINE toValue #-}
    toValue x = Object (V.pack [("numerator", n), ("denominator", d)])
      where !n = toValue (numerator x)
            !d = toValue (denominator x)
instance (EncodeJSON a, Integral a) => EncodeJSON (Ratio a) where
    {-# INLINE encodeJSON #-}
    encodeJSON x =
        B.curly $ ("\"numerator\""   >> B.colon >> encodeJSON (numerator x))
            >> B.comma >> ("\"denominator\"" >> B.colon >> encodeJSON (denominator x))

-- | This instance includes a bounds check to prevent maliciously large inputs to fill up the memory of the target system. You can newtype Fixed and provide your own instance using 'withScientific' if you want to allow larger inputs.
instance HasResolution a => FromValue (Fixed a) where
    {-# INLINE fromValue #-}
    fromValue = withBoundedScientific "Fixed" (pure . realToFrac)
instance HasResolution a => ToValue (Fixed a) where
    {-# INLINE toValue #-}
    toValue = Number . realToFrac
instance HasResolution a => EncodeJSON (Fixed a) where
    {-# INLINE encodeJSON #-}
    encodeJSON = B.scientific . realToFrac

--------------------------------------------------------------------------------

deriving newtype instance FromValue (f (g a)) => FromValue (Compose f g a)
deriving newtype instance FromValue a => FromValue (Semigroup.Min a)
deriving newtype instance FromValue a => FromValue (Semigroup.Max a)
deriving newtype instance FromValue a => FromValue (Semigroup.First a)
deriving newtype instance FromValue a => FromValue (Semigroup.Last a)
deriving newtype instance FromValue a => FromValue (Semigroup.WrappedMonoid a)
deriving newtype instance FromValue a => FromValue (Semigroup.Dual a)
deriving newtype instance FromValue a => FromValue (Monoid.First a)
deriving newtype instance FromValue a => FromValue (Monoid.Last a)
deriving newtype instance FromValue a => FromValue (Identity a)
deriving newtype instance FromValue a => FromValue (Const a b)
deriving newtype instance FromValue b => FromValue (Tagged a b)

deriving newtype instance ToValue (f (g a)) => ToValue (Compose f g a)
deriving newtype instance ToValue a => ToValue (Semigroup.Min a)
deriving newtype instance ToValue a => ToValue (Semigroup.Max a)
deriving newtype instance ToValue a => ToValue (Semigroup.First a)
deriving newtype instance ToValue a => ToValue (Semigroup.Last a)
deriving newtype instance ToValue a => ToValue (Semigroup.WrappedMonoid a)
deriving newtype instance ToValue a => ToValue (Semigroup.Dual a)
deriving newtype instance ToValue a => ToValue (Monoid.First a)
deriving newtype instance ToValue a => ToValue (Monoid.Last a)
deriving newtype instance ToValue a => ToValue (Identity a)
deriving newtype instance ToValue a => ToValue (Const a b)
deriving newtype instance ToValue b => ToValue (Tagged a b)

deriving newtype instance EncodeJSON (f (g a)) => EncodeJSON (Compose f g a)
deriving newtype instance EncodeJSON a => EncodeJSON (Semigroup.Min a)
deriving newtype instance EncodeJSON a => EncodeJSON (Semigroup.Max a)
deriving newtype instance EncodeJSON a => EncodeJSON (Semigroup.First a)
deriving newtype instance EncodeJSON a => EncodeJSON (Semigroup.Last a)
deriving newtype instance EncodeJSON a => EncodeJSON (Semigroup.WrappedMonoid a)
deriving newtype instance EncodeJSON a => EncodeJSON (Semigroup.Dual a)
deriving newtype instance EncodeJSON a => EncodeJSON (Monoid.First a)
deriving newtype instance EncodeJSON a => EncodeJSON (Monoid.Last a)
deriving newtype instance EncodeJSON a => EncodeJSON (Identity a)
deriving newtype instance EncodeJSON a => EncodeJSON (Const a b)
deriving newtype instance EncodeJSON b => EncodeJSON (Tagged a b)

--------------------------------------------------------------------------------

deriving anyclass instance (FromValue (f a), FromValue (g a), FromValue a) => FromValue (Sum f g a)
deriving anyclass instance (FromValue a, FromValue b) => FromValue (Either a b)
deriving anyclass instance (FromValue (f a), FromValue (g a)) => FromValue (Product f g a)
deriving anyclass instance (FromValue a, FromValue b) => FromValue (a, b)
deriving anyclass instance (FromValue a, FromValue b, FromValue c) => FromValue (a, b, c)
deriving anyclass instance (FromValue a, FromValue b, FromValue c, FromValue d) => FromValue (a, b, c, d)
deriving anyclass instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e) => FromValue (a, b, c, d, e)
deriving anyclass instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e, FromValue f) => FromValue (a, b, c, d, e, f)
deriving anyclass instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e, FromValue f, FromValue g) => FromValue (a, b, c, d, e, f, g)

deriving anyclass instance (ToValue (f a), ToValue (g a), ToValue a) => ToValue (Sum f g a)
deriving anyclass instance (ToValue a, ToValue b) => ToValue (Either a b)
deriving anyclass instance (ToValue (f a), ToValue (g a)) => ToValue (Product f g a)
deriving anyclass instance (ToValue a, ToValue b) => ToValue (a, b)
deriving anyclass instance (ToValue a, ToValue b, ToValue c) => ToValue (a, b, c)
deriving anyclass instance (ToValue a, ToValue b, ToValue c, ToValue d) => ToValue (a, b, c, d)
deriving anyclass instance (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e) => ToValue (a, b, c, d, e)
deriving anyclass instance (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e, ToValue f) => ToValue (a, b, c, d, e, f)
deriving anyclass instance (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e, ToValue f, ToValue g) => ToValue (a, b, c, d, e, f, g)

deriving anyclass instance (EncodeJSON (f a), EncodeJSON (g a), EncodeJSON a) => EncodeJSON (Sum f g a)
deriving anyclass instance (EncodeJSON a, EncodeJSON b) => EncodeJSON (Either a b)
deriving anyclass instance (EncodeJSON (f a), EncodeJSON (g a)) => EncodeJSON (Product f g a)
deriving anyclass instance (EncodeJSON a, EncodeJSON b) => EncodeJSON (a, b)
deriving anyclass instance (EncodeJSON a, EncodeJSON b, EncodeJSON c) => EncodeJSON (a, b, c)
deriving anyclass instance (EncodeJSON a, EncodeJSON b, EncodeJSON c, EncodeJSON d) => EncodeJSON (a, b, c, d)
deriving anyclass instance (EncodeJSON a, EncodeJSON b, EncodeJSON c, EncodeJSON d, EncodeJSON e) => EncodeJSON (a, b, c, d, e)
deriving anyclass instance (EncodeJSON a, EncodeJSON b, EncodeJSON c, EncodeJSON d, EncodeJSON e, EncodeJSON f) => EncodeJSON (a, b, c, d, e, f)
deriving anyclass instance (EncodeJSON a, EncodeJSON b, EncodeJSON c, EncodeJSON d, EncodeJSON e, EncodeJSON f, EncodeJSON g) => EncodeJSON (a, b, c, d, e, f, g)
