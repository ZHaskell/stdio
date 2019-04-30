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

-}

module Std.Data.JSON.Base where

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
import qualified Data.Monoid                  as Monoid
import           Data.Primitive.Types         (Prim)
import qualified Data.Primitive.SmallArray    as A
import           Data.Proxy                   (Proxy (..))
import           Data.Ratio                   (Ratio, (%))
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
import qualified Std.Data.JSON.Value.Builder  as JB
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

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define COLON 58
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91

--------------------------------------------------------------------------------

-- There're two possible failures here:
--
--   * 'P.ParseError' is an error during parsing bytes to 'Value'.
--   * 'ConvertError' is an error when converting 'Value' to target data type.
type DecodeError = Either P.ParseError ConvertError

-- | Decode a JSON doc, only trailing JSON whitespace are allowed.
--
decode' :: FromJSON a => V.Bytes -> Either DecodeError a
{-# INLINE decode' #-}
decode' bs = case P.parse_ (JV.value <* JV.skipSpaces <* P.endOfInput) bs of
    Left pErr -> Left (Left pErr)
    Right v -> case convert fromJSON v of
        Left cErr -> Left (Right cErr)
        Right r -> Right r

-- | Decode a JSON bytes, return any trailing bytes.
decode :: FromJSON a => V.Bytes -> (V.Bytes, Either DecodeError a)
{-# INLINE decode #-}
decode bs = case P.parse JV.value bs of
    (bs', Left pErr) -> (bs', Left (Left pErr))
    (bs', Right v) -> case convert fromJSON v of
        Left cErr -> (bs', Left (Right cErr))
        Right r -> (bs', Right r)

encodeBytes :: EncodeJSON a => a -> V.Bytes
{-# INLINE encodeBytes #-}
encodeBytes = B.buildBytes . encodeJSON

encodeText :: EncodeJSON a => a -> T.Text
{-# INLINE encodeText #-}
encodeText = TB.buildText . TB.unsafeFromBuilder . encodeJSON

-- | Decode a JSON doc chunks, return trailing bytes.
decodeChunks :: (FromJSON a, Monad m) => m V.Bytes -> V.Bytes -> m (V.Bytes, Either DecodeError a)
{-# INLINE decodeChunks #-}
decodeChunks mb bs = do
    mr <- (P.parseChunks JV.value mb bs)
    case mr of
        (bs', Left pErr) -> pure (bs', Left (Left pErr))
        (bs', Right v) -> case convert fromJSON v of
            Left cErr -> pure (bs', Left (Right cErr))
            Right r -> pure (bs', Right r)

-- | Run a 'Converter' with input value.
convert :: (a -> Converter r) -> a -> Either ConvertError r
{-# INLINE convert #-}
convert m v = runConverter (m v) (\ paths msg -> (Left (ConvertError paths msg))) Right

-- | Run a 'Converter' with input value.
convert' :: (FromJSON a) => Value -> Either ConvertError a
{-# INLINE convert' #-}
convert' = convert fromJSON

--------------------------------------------------------------------------------

-- | Elements of a JSON path used to describe the location of an error.
data PathElement
    = Key {-# UNPACK #-} !T.Text
        -- ^ JSON path element of a key into an object,
        -- \"object.key\".
    | Index {-# UNPACK #-} !Int
        -- ^ JSON path element of an index into an
        -- array, \"array[index]\".
    | Embedded
        -- ^ JSON path of a embedded JSON String
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
        renderPath (Key k) = TB.char7 '.' >> TB.text k
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
(.:) :: (FromJSON a) => FM.FlatMap T.Text Value -> T.Text -> Converter a
{-# INLINE (.:) #-}
(.:) = convertField fromJSON

-- | Retrieve the value associated with the given key of an 'Object'. The
-- result is 'Nothing' if the key is not present or if its value is 'Null',
-- or 'empty' if the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
(.:?) :: (FromJSON a) => FM.FlatMap T.Text Value -> T.Text -> Converter (Maybe a)
{-# INLINE (.:?) #-}
(.:?) = convertFieldMaybe fromJSON

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present or 'empty' if the
-- value cannot be converted to the desired type.
--
-- This differs from '.:?' by attempting to convert 'Null' the same as any
-- other JSON value, instead of interpreting it as 'Nothing'.
(.:!) :: (FromJSON a) => FM.FlatMap T.Text Value -> T.Text -> Converter (Maybe a)
{-# INLINE (.:!) #-}
(.:!) = convertFieldMaybe' fromJSON

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

-- | Generic encode/decode Settings
--
-- There should be no control charactors in formatted texts since we don't escaping those
-- field names or constructor names ('defaultSetting' relys on Haskell's lexical property).
-- Otherwise 'encodeJSON' will output illegal JSON string.
data Settings = Settings
    { fieldFmt :: String -> T.Text  -- ^ format field labels
    , constrFmt :: String -> T.Text -- ^ format constructor names.
    }

defaultSetting :: Settings
defaultSetting = Settings T.pack T.pack

--------------------------------------------------------------------------------
-- ToJSON
--------------------------------------------------------------------------------

-- | Typeclass for converting to JSON 'Value'.
class ToJSON a where
    toJSON :: a -> Value
    default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
    toJSON = gToJSON defaultSetting . from

class GToJSON f where
    gToJSON :: Settings -> f a -> Value


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

instance (GToJSON f) => GWriteFields (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gWriteFields #-}
    gWriteFields s marr idx (M1 x) = A.writeSmallArray marr idx (gToJSON s x)

instance (GToJSON f, Selector (MetaSel (Just l) u ss ds)) => GWriteFields (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gWriteFields #-}
    gWriteFields s marr idx m1@(M1 x) = A.writeSmallArray marr idx ((fieldFmt s) (selName m1), gToJSON s x)

instance (GToJSON f, Selector (MetaSel (Just l) u ss ds)) => GToJSON (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gToJSON #-}
    gToJSON s m1@(M1 x) =
        let k = fieldFmt s $ selName m1
            v = gToJSON s x
        in Object (V.singleton (k, v))

instance GToJSON f => GToJSON (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gToJSON #-}
    gToJSON s (M1 x) = gToJSON s x

instance ToJSON a => GToJSON (K1 i a) where
    {-# INLINE gToJSON #-}
    gToJSON s (K1 x) = toJSON x

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

class GConstrToJSON f where
    gConstrToJSON :: Bool -> Settings -> f a -> Value

instance GConstrToJSON V1 where
    {-# INLINE gConstrToJSON #-}
    gConstrToJSON _ _ _ = error "Std.Data.JSON.Base: empty data type"

instance (GConstrToJSON f, GConstrToJSON g) => GConstrToJSON (f :+: g) where
    {-# INLINE gConstrToJSON #-}
    gConstrToJSON _ s (L1 x) = gConstrToJSON True s x
    gConstrToJSON _ s (R1 x) = gConstrToJSON True s x

-- | Constructor without payload, convert to String
instance (Constructor c) => GConstrToJSON (C1 c U1) where
    {-# INLINE gConstrToJSON #-}
    gConstrToJSON _ s (M1 _) = String . constrFmt s $ conName (undefined :: t c U1 a)

-- | Constructor with a single payload
instance (Constructor c, GToJSON (S1 sc f)) => GConstrToJSON (C1 c (S1 sc f)) where
    {-# INLINE gConstrToJSON #-}
    gConstrToJSON False s (M1 x) = gToJSON s x
    gConstrToJSON True s (M1 x) =
        let k = constrFmt s $ conName (undefined :: t c f a)
            v = gToJSON s x
        in Object (V.singleton (k, v))

-- | Constructor with multiple payloads
instance (ProductSize (a :*: b), GWriteFields (a :*: b), GMergeFields (a :*: b), Constructor c)
    => GConstrToJSON (C1 c (a :*: b)) where
    {-# INLINE gConstrToJSON #-}
    gConstrToJSON False s (M1 x) = runST (do
        marr <- A.newSmallArray (productSize (proxy# :: Proxy# (a :*: b))) undefined
        gWriteFields s marr 0 x
        gMergeFields (proxy# :: Proxy# (a :*: b)) marr)
    gConstrToJSON True s (M1 x) =
        let k = constrFmt s $ conName (undefined :: t c f a)
            v = runST (do
                    marr <- A.newSmallArray (productSize (proxy# :: Proxy# (a :*: b))) undefined
                    gWriteFields s marr 0 x
                    gMergeFields (proxy# :: Proxy# (a :*: b)) marr)
        in Object (V.singleton (k, v))

--------------------------------------------------------------------------------
-- Data types
instance GConstrToJSON f => GToJSON (D1 c f) where
    {-# INLINE gToJSON #-}
    gToJSON s (M1 x) = gConstrToJSON False s x


--------------------------------------------------------------------------------
-- EncodeJSON
--------------------------------------------------------------------------------

class EncodeJSON a where
    encodeJSON :: a -> B.Builder ()
    default encodeJSON :: (Generic a, GEncodeJSON (Rep a)) => a -> B.Builder ()
    encodeJSON = gEncodeJSON defaultSetting . from

encodeJSONText :: EncodeJSON a => a -> TB.TextBuilder ()
{-# INLINE encodeJSONText #-}
encodeJSONText = TB.unsafeFromBuilder . encodeJSON


class GEncodeJSON f where
    gEncodeJSON :: Settings -> f a -> B.Builder ()

--------------------------------------------------------------------------------
-- Selectors

instance (GEncodeJSON f, Selector (MetaSel (Just l) u ss ds)) => GEncodeJSON (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s m1@(M1 x) = do
        B.encodePrim @Word8 DOUBLE_QUOTE
        B.text . fieldFmt s $ selName m1
        B.encodePrim @Word8 DOUBLE_QUOTE
        B.encodePrim @Word8 COLON
        gEncodeJSON s x

instance GEncodeJSON f => GEncodeJSON (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s (M1 x) = gEncodeJSON s x

instance (GEncodeJSON a, GEncodeJSON b) => GEncodeJSON (a :*: b) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s (a :*: b) = do
        gEncodeJSON s a
        B.encodePrim @Word8 COMMA
        gEncodeJSON s b

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
    gAddPunctuation _ b = B.encodePrim @Word8 OPEN_SQUARE >> b >> B.encodePrim @Word8 CLOSE_SQUARE

instance GAddPunctuation (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gAddPunctuation #-}
    gAddPunctuation _ b = B.encodePrim @Word8 OPEN_CURLY >> b >> B.encodePrim @Word8 CLOSE_CURLY

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
    gConstrEncodeJSON _ s (M1 _) = do
        B.encodePrim @Word8 DOUBLE_QUOTE
        B.text . constrFmt s $ conName (undefined :: t c U1 a)
        B.encodePrim @Word8 DOUBLE_QUOTE

-- | Constructor with a single payload
instance (Constructor c, GEncodeJSON (S1 sc f)) => GConstrEncodeJSON (C1 c (S1 sc f)) where
    {-# INLINE gConstrEncodeJSON #-}
    gConstrEncodeJSON False s (M1 x) = gEncodeJSON s x
    gConstrEncodeJSON True s (M1 x) = do
        B.encodePrim @Word8 OPEN_CURLY
        B.encodePrim @Word8 DOUBLE_QUOTE
        B.text . constrFmt s $ conName (undefined :: t c f a)
        B.encodePrim @Word8 DOUBLE_QUOTE
        B.encodePrim @Word8 COLON
        gEncodeJSON s x
        B.encodePrim @Word8 CLOSE_CURLY

-- | Constructor with multiple payloads
instance (GEncodeJSON (a :*: b), GAddPunctuation (a :*: b), Constructor c)
    => GConstrEncodeJSON (C1 c (a :*: b)) where
    {-# INLINE gConstrEncodeJSON #-}
    gConstrEncodeJSON False s (M1 x) = gAddPunctuation (proxy# :: Proxy# (a :*: b)) (gEncodeJSON s x)
    gConstrEncodeJSON True s (M1 x) = do
        B.encodePrim @Word8 OPEN_CURLY
        B.encodePrim @Word8 DOUBLE_QUOTE
        B.text . constrFmt s $ conName (undefined :: t c f a)
        B.encodePrim @Word8 DOUBLE_QUOTE
        B.encodePrim @Word8 COLON
        gAddPunctuation (proxy# :: Proxy# (a :*: b)) (gEncodeJSON s x)
        B.encodePrim @Word8 CLOSE_CURLY

--------------------------------------------------------------------------------
-- Data types
instance GConstrEncodeJSON f => GEncodeJSON (D1 c f) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s (M1 x) = gConstrEncodeJSON False s x

--------------------------------------------------------------------------------
-- FromJSON
--------------------------------------------------------------------------------

class FromJSON a where
    fromJSON :: Value -> Converter a
    default fromJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Converter a
    fromJSON v = to <$> gFromJSON defaultSetting v

class GFromJSON f where
    gFromJSON :: Settings -> Value -> Converter (f a)


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

instance (GFromJSON f) => GFromFields (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFromFields #-}
    gFromFields s v idx = do
        v' <- V.unsafeIndexM v idx
        M1 <$> gFromJSON s v' <?> Index idx

instance (GFromJSON f, Selector (MetaSel (Just l) u ss ds)) => GFromFields (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gFromFields #-}
    gFromFields s v _ = do
        case FM.lookup fn v of
            Just v' -> M1 <$> gFromJSON s v' <?> Key fn
            _       -> fail' ("Std.Data.JSON.Base: missing field " <>  fn)
      where
        fn = (fieldFmt s) (selName (undefined :: S1 (MetaSel (Just l) u ss ds) f a))

instance GFromJSON f => GFromJSON (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFromJSON #-}
    gFromJSON s x = M1 <$> gFromJSON s x

instance (GFromJSON f, Selector (MetaSel (Just l) u ss ds)) => GFromJSON (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gFromJSON #-}
    gFromJSON s (Object v) = do
        case FM.lookup fn (FM.packVectorR v) of
            Just v' -> M1 <$> gFromJSON s v' <?> Key fn
            _       -> fail' ("Std.Data.JSON.Base: missing field " <>  fn)
      where fn = (fieldFmt s) (selName (undefined :: S1 (MetaSel (Just l) u ss ds) f a))
    gFromJSON s v = typeMismatch ("field " <> fn) "Object" v <?> Key fn
      where fn = (fieldFmt s) (selName (undefined :: S1 (MetaSel (Just l) u ss ds) f a))

instance FromJSON a => GFromJSON (K1 i a) where
    {-# INLINE gFromJSON #-}
    gFromJSON s x = K1 <$> fromJSON x

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

class GConstrFromJSON f where
    gConstrFromJSON :: Bool -> Settings -> Value -> Converter (f a)

instance GConstrFromJSON V1 where
    {-# INLINE gConstrFromJSON #-}
    gConstrFromJSON _ _ _ = error "Std.Data.JSON.Base: empty data type"

instance (GConstrFromJSON f, GConstrFromJSON g) => GConstrFromJSON (f :+: g) where
    {-# INLINE gConstrFromJSON #-}
    gConstrFromJSON _ s x = (L1 <$> gConstrFromJSON True s x) <|> (R1 <$> gConstrFromJSON True s x)

-- | Constructor without payload, convert to String
instance (Constructor c) => GConstrFromJSON (C1 c U1) where
    {-# INLINE gConstrFromJSON #-}
    gConstrFromJSON _ s (String x)
        | cn == x   = pure (M1 U1)
        | otherwise = fail' . T.concat $ ["converting ", cn', "failed, unknown constructor name ", x]
      where cn = constrFmt s $ conName (undefined :: t c U1 a)
            cn' = T.pack $ conName (undefined :: t c U1 a)
    gConstrFromJSON _ _ v = typeMismatch cn' "String" v
      where cn' = T.pack $ conName (undefined :: t c U1 a)

-- | Constructor with a single payload
instance (Constructor c, GFromJSON (S1 sc f)) => GConstrFromJSON (C1 c (S1 sc f)) where
    {-# INLINE gConstrFromJSON #-}
    gConstrFromJSON False s x = M1 <$> gFromJSON s x
    gConstrFromJSON True s x = case x of
        Object v -> case V.indexM v 0 of
            Just (k, v') | k == cn -> M1 <$> gFromJSON s v' <?> Key cn
            _                      -> fail' .T.concat $ ["converting ", cn', " failed, constructor not found"]
        _ ->  typeMismatch cn' "Object" x
      where cn = constrFmt s $ conName (undefined :: t c f a)
            cn' = T.pack $ conName (undefined :: t c f a)

-- | Constructor with multiple payloads
instance (ProductSize (a :*: b), GFromFields (a :*: b), GBuildLookup (a :*: b), Constructor c)
    => GConstrFromJSON (C1 c (a :*: b)) where
    {-# INLINE gConstrFromJSON #-}
    gConstrFromJSON False s x = do
        t <- gBuildLookup p (productSize p) cn' x
        M1 <$> gFromFields s t 0
      where cn' = T.pack $ conName (undefined :: t c f a)
            p = proxy# :: Proxy# (a :*: b)
    gConstrFromJSON True s x = case x of
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
instance GConstrFromJSON f => GFromJSON (D1 c f) where
    {-# INLINE gFromJSON #-}
    gFromJSON s x = M1 <$> gConstrFromJSON False s x

--------------------------------------------------------------------------------
-- Built-in Instances
--------------------------------------------------------------------------------
-- | Use 'Null' as @Proxy a@
instance FromJSON (Proxy a)   where {{-# INLINE fromJSON #-}; fromJSON = fromNull "Proxy" Proxy;}
instance ToJSON (Proxy a)     where {{-# INLINE toJSON #-}; toJSON _ = Null;}
instance EncodeJSON (Proxy a) where {{-# INLINE encodeJSON #-}; encodeJSON _ = "null";}

instance FromJSON Value   where {{-# INLINE fromJSON #-}; fromJSON = pure;}
instance ToJSON Value     where { {-# INLINE toJSON #-}; toJSON = id; }
instance EncodeJSON Value where { {-# INLINE encodeJSON #-}; encodeJSON = JB.value; }

instance FromJSON T.Text   where {{-# INLINE fromJSON #-}; fromJSON = withText "Text" pure;}
instance ToJSON T.Text     where {{-# INLINE toJSON #-}; toJSON = String;}
instance EncodeJSON T.Text where {{-# INLINE encodeJSON #-}; encodeJSON = JB.string;}

instance FromJSON Scientific where {{-# INLINE fromJSON #-}; fromJSON = withScientific "Scientific" pure;}
instance ToJSON Scientific where {{-# INLINE toJSON #-}; toJSON = Number;}
instance EncodeJSON Scientific where {{-# INLINE encodeJSON #-}; encodeJSON = B.scientific;}

-- | default instance prefer later key
instance FromJSON a => FromJSON (FM.FlatMap T.Text a) where
    {-# INLINE fromJSON #-}
    fromJSON = withFlatMapR "Std.Data.Vector.FlatMap.FlatMap"
        (FM.traverseWithKey $ \ k v -> fromJSON v <?> Key k)
instance ToJSON a => ToJSON (FM.FlatMap T.Text a) where
    {-# INLINE toJSON #-}
    toJSON = Object . FM.sortedKeyValues . FM.map' toJSON
instance EncodeJSON a => EncodeJSON (FM.FlatMap T.Text a) where
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.object' encodeJSON . FM.sortedKeyValues

instance (Ord a, FromJSON a) => FromJSON (FS.FlatSet a) where
    {-# INLINE fromJSON #-}
    fromJSON = withArray "Std.Data.Vector.FlatSet.FlatSet" $ \ v ->
        FS.packRN (V.length v) <$>
            (zipWithM (\ k v -> fromJSON v <?> Index k) [0..] (V.unpack v))
instance ToJSON a => ToJSON (FS.FlatSet a) where
    {-# INLINE toJSON #-}
    toJSON = Array . V.map' toJSON . FS.sortedValues
instance EncodeJSON a => EncodeJSON (FS.FlatSet a) where
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.array' encodeJSON . FS.sortedValues

-- | default instance prefer later key
instance FromJSON a => FromJSON (HM.HashMap T.Text a) where
    {-# INLINE fromJSON #-}
    fromJSON = withHashMapR "Data.HashMap.HashMap"
        (HM.traverseWithKey $ \ k v -> fromJSON v <?> Key k)
instance ToJSON a => ToJSON (HM.HashMap T.Text a) where
    {-# INLINE toJSON #-}
    toJSON = Object . V.pack . HM.toList . HM.map toJSON
instance EncodeJSON a => EncodeJSON (HM.HashMap T.Text a) where
    {-# INLINE encodeJSON #-}
    encodeJSON m
        | HM.null m = "{}"
        | otherwise = do
            let (x:xs) = HM.toList m
            B.encodePrim @Word8 OPEN_CURLY
            JB.string (fst x) >> B.encodePrim @Word8 COLON >> encodeJSON (snd x)
            forM xs $ \ x ->
                B.encodePrim @Word8 COMMA >> JB.string (fst x) >> B.encodePrim @Word8 COLON >> encodeJSON (snd x)
            B.encodePrim @Word8 CLOSE_CURLY

instance FromJSON a => FromJSON (FIM.FlatIntMap a) where
    {-# INLINE fromJSON #-}
    fromJSON = withFlatMapR "Std.Data.Vector.FlatIntMap.FlatIntMap" $ \ m ->
        let kvs = FM.sortedKeyValues m
        in FIM.packVectorR <$> (forM kvs $ \ (k, v) -> do
            case P.parse_ P.int (T.getUTF8Bytes k) of
                Right k' -> do
                    v' <- fromJSON v <?> Key k
                    return (V.IPair k' v')
                _ -> fail' ("converting Std.Data.Vector.FlatIntMap.FlatIntMap failed, unexpected key " <> k))
instance ToJSON a => ToJSON (FIM.FlatIntMap a) where
    {-# INLINE toJSON #-}
    toJSON = Object . V.map' toKV . FIM.sortedKeyValues
      where toKV (V.IPair i x) = let !k = TB.buildText (TB.int i)
                                     !v = toJSON x
                                 in (k, v)
instance EncodeJSON a => EncodeJSON (FIM.FlatIntMap a) where
    {-# INLINE encodeJSON #-}
    encodeJSON m = do
        let ips = FIM.sortedKeyValues m
        B.encodePrim @Word8 OPEN_CURLY
        forM_ (V.initMayEmpty ips) $ \ (V.IPair i x) -> do
            B.encodePrim @Word8 DOUBLE_QUOTE
            B.int i
            B.encodePrim @Word8 DOUBLE_QUOTE
            B.encodePrim @Word8 COLON
            encodeJSON x
            B.encodePrim @Word8 COMMA
        forM_ (V.lastMaybe ips) $ \ (V.IPair i x) -> do
            B.encodePrim @Word8 DOUBLE_QUOTE
            B.int i
            B.encodePrim @Word8 DOUBLE_QUOTE
            B.encodePrim @Word8 COLON
            encodeJSON x
        B.encodePrim @Word8 CLOSE_CURLY

instance FromJSON FIS.FlatIntSet where
    {-# INLINE fromJSON #-}
    fromJSON = withArray "Std.Data.Vector.FlatIntSet.FlatIntSet" $ \ v ->
        FIS.packRN (V.length v) <$>
            (zipWithM (\ k v -> fromJSON v <?> Index k) [0..] (V.unpack v))

instance FromJSON a => FromJSON (V.Vector a) where
    {-# INLINE fromJSON #-}
    fromJSON = withArray "Std.Data.Vector.Vector"
        (V.traverseWithIndex $ \ k v -> fromJSON v <?> Index k)

instance (Prim a, FromJSON a) => FromJSON (V.PrimVector a) where
    {-# INLINE fromJSON #-}
    fromJSON = withArray "Std.Data.Vector.PrimVector"
        (V.traverseWithIndex $ \ k v -> fromJSON v <?> Index k)

instance (Eq a, Hashable a, FromJSON a) => FromJSON (HS.HashSet a) where
    {-# INLINE fromJSON #-}
    fromJSON = withArray "Std.Data.Vector.FlatSet.FlatSet" $ \ v ->
        HS.fromList <$>
            (zipWithM (\ k v -> fromJSON v <?> Index k) [0..] (V.unpack v))

instance {-# OVERLAPPABLE #-} FromJSON a => FromJSON [a] where
    {-# INLINE fromJSON #-}
    fromJSON = withArray "[a]" $ \ v ->
        zipWithM (\ k v -> fromJSON v <?> Index k) [0..] (V.unpack v)
instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON [a] where
    {-# INLINE toJSON #-}
    toJSON = Array . V.pack . map toJSON
instance {-# OVERLAPPABLE #-} EncodeJSON a => EncodeJSON [a] where
    {-# INLINE encodeJSON #-}
    encodeJSON xs = do
        B.encodePrim @Word8 OPEN_SQUARE
        go xs
        B.encodePrim @Word8 CLOSE_SQUARE
      where
        go [] = pure ()
        go [x] = encodeJSON x
        go (x:xs) = encodeJSON x >> B.encodePrim @Word8 COMMA >> go xs

instance {-# OVERLAPPING #-} FromJSON String where
    {-# INLINE fromJSON #-}
    fromJSON = withText "String" (pure . T.unpack)
instance {-# OVERLAPPING #-} ToJSON String where
    {-# INLINE toJSON #-}
    toJSON = String . T.pack
instance {-# OVERLAPPING #-} EncodeJSON String where
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.string . T.pack

instance FromJSON a => FromJSON (NonEmpty a) where
    {-# INLINE fromJSON #-}
    fromJSON = withArray "NonEmpty" $ \ v -> do
        l <- zipWithM (\ k v -> fromJSON v <?> Index k) [0..] (V.unpack v)
        case l of (x:xs) -> pure (x :| xs)
                  _      -> fail' "unexpected empty array"

instance FromJSON Bool where {{-# INLINE fromJSON #-}; fromJSON = withBool "Bool" pure;}
instance ToJSON Bool where {{-# INLINE toJSON #-}; toJSON = Bool; }
instance EncodeJSON Bool where {{-# INLINE encodeJSON #-}; encodeJSON True = "true"; encodeJSON _ = "false";}

instance FromJSON Char where
    {-# INLINE fromJSON #-}
    fromJSON = withText "Char" $ \ t ->
        case T.headMaybe t of
            Just c -> pure c
            _      -> fail' (T.concat ["converting Char failed, expected a string of length 1"])
instance ToJSON Char where
    {-# INLINE toJSON #-}
    toJSON = String . T.singleton
instance EncodeJSON Char where
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.string . T.singleton


instance FromJSON Double where {{-# INLINE fromJSON #-}; fromJSON = withRealFloat "Double" pure;}
instance FromJSON Float  where {{-# INLINE fromJSON #-}; fromJSON = withRealFloat "Double" pure;}
instance ToJSON Float  where {{-# INLINE toJSON #-}; toJSON = Number . P.floatToScientific;}
instance ToJSON Double where {{-# INLINE toJSON #-}; toJSON = Number . P.doubleToScientific;}
instance EncodeJSON Float  where {{-# INLINE encodeJSON #-}; encodeJSON = B.float;}
instance EncodeJSON Double where {{-# INLINE encodeJSON #-}; encodeJSON = B.double;}

instance FromJSON Int    where {{-# INLINE fromJSON #-}; fromJSON = withBoundedIntegral "Int" pure;}
instance FromJSON Int8   where {{-# INLINE fromJSON #-}; fromJSON = withBoundedIntegral "Int8" pure;}
instance FromJSON Int16  where {{-# INLINE fromJSON #-}; fromJSON = withBoundedIntegral "Int16" pure;}
instance FromJSON Int32  where {{-# INLINE fromJSON #-}; fromJSON = withBoundedIntegral "Int32" pure;}
instance FromJSON Int64  where {{-# INLINE fromJSON #-}; fromJSON = withBoundedIntegral "Int64" pure;}
instance FromJSON Word   where {{-# INLINE fromJSON #-}; fromJSON = withBoundedIntegral "Word" pure;}
instance FromJSON Word8  where {{-# INLINE fromJSON #-}; fromJSON = withBoundedIntegral "Word8" pure;}
instance FromJSON Word16 where {{-# INLINE fromJSON #-}; fromJSON = withBoundedIntegral "Word16" pure;}
instance FromJSON Word32 where {{-# INLINE fromJSON #-}; fromJSON = withBoundedIntegral "Word32" pure;}
instance FromJSON Word64 where {{-# INLINE fromJSON #-}; fromJSON = withBoundedIntegral "Word64" pure;}
instance ToJSON Int    where {{-# INLINE toJSON #-}; toJSON = Number . fromIntegral;}
instance ToJSON Int8   where {{-# INLINE toJSON #-}; toJSON = Number . fromIntegral;}
instance ToJSON Int16  where {{-# INLINE toJSON #-}; toJSON = Number . fromIntegral;}
instance ToJSON Int32  where {{-# INLINE toJSON #-}; toJSON = Number . fromIntegral;}
instance ToJSON Int64  where {{-# INLINE toJSON #-}; toJSON = Number . fromIntegral;}
instance ToJSON Word   where {{-# INLINE toJSON #-}; toJSON = Number . fromIntegral;}
instance ToJSON Word8  where {{-# INLINE toJSON #-}; toJSON = Number . fromIntegral;}
instance ToJSON Word16 where {{-# INLINE toJSON #-}; toJSON = Number . fromIntegral;}
instance ToJSON Word32 where {{-# INLINE toJSON #-}; toJSON = Number . fromIntegral;}
instance ToJSON Word64 where {{-# INLINE toJSON #-}; toJSON = Number . fromIntegral;}
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
instance FromJSON Integer where
    {-# INLINE fromJSON #-}
    fromJSON = withBoundedScientific "Integer" $ \ n ->
        case Scientific.floatingOrInteger n :: Either Double Integer of
            Right x -> pure x
            Left _  -> fail' . TB.buildText $ do
                "converting Integer failed, unexpected floating number "
                TB.scientific n

instance FromJSON Natural where
    {-# INLINE fromJSON #-}
    fromJSON = withBoundedScientific "Natural" $ \ n ->
        if n < 0
        then fail' . TB.buildText $ do
                "converting Natural failed, unexpected negative number "
                TB.scientific n
        else case Scientific.floatingOrInteger n :: Either Double Natural of
            Right x -> pure x
            Left _  -> fail' . TB.buildText $ do
                "converting Natural failed, unexpected floating number "
                TB.scientific n

instance FromJSON Ordering where
    fromJSON = withText "Ordering" $ \ s ->
        case s of
            "LT" -> pure LT
            "EQ" -> pure EQ
            "GT" -> pure GT
            _ -> fail' . T.concat $ ["converting Ordering failed, unexpected ",
                                        s, " expected \"LT\", \"EQ\", or \"GT\""]

instance FromJSON () where
    {-# INLINE fromJSON #-}
    fromJSON = withArray "()" $ \ v ->
        if V.null v
        then pure ()
        else fail' "converting () failed, expected an empty array"
instance ToJSON () where
    {-# INLINE toJSON #-}
    toJSON () = Array V.empty
instance EncodeJSON () where
    {-# INLINE encodeJSON #-}
    encodeJSON () = "[]"

instance FromJSON Version where
    {-# INLINE fromJSON #-}
    fromJSON = withText "Version" (go . readP_to_S parseVersion . T.unpack)
      where
        go [(v,[])] = pure v
        go (_ : xs) = go xs
        go _        = fail "converting Version failed"

instance FromJSON a => FromJSON (Maybe a) where
    {-# INLINE fromJSON #-}
    fromJSON Null = pure Nothing
    fromJSON v = Just <$> fromJSON v

-- | This instance includes a bounds check to prevent maliciously large inputs to fill up the memory of the target system. You can newtype Ratio and provide your own instance using 'withScientific' if you want to allow larger inputs.
instance (FromJSON a, Integral a) => FromJSON (Ratio a) where
    {-# INLINE fromJSON #-}
    fromJSON = withFlatMapR "Rational" $ \obj -> do
        numerator <- obj .: "numerator"
        denominator <- obj .: "denominator"
        if denominator == 0
        then fail' "Ratio denominator was 0"
        else pure (numerator % denominator)

-- | This instance includes a bounds check to prevent maliciously large inputs to fill up the memory of the target system. You can newtype Fixed and provide your own instance using 'withScientific' if you want to allow larger inputs.
instance HasResolution a => FromJSON (Fixed a) where
    {-# INLINE fromJSON #-}
    fromJSON = withBoundedScientific "Fixed" (pure . realToFrac)

deriving newtype instance FromJSON (f (g a)) => FromJSON (Compose f g a)
deriving newtype instance FromJSON a => FromJSON (Semigroup.Min a)
deriving newtype instance FromJSON a => FromJSON (Semigroup.Max a)
deriving newtype instance FromJSON a => FromJSON (Semigroup.First a)
deriving newtype instance FromJSON a => FromJSON (Semigroup.Last a)
deriving newtype instance FromJSON a => FromJSON (Semigroup.WrappedMonoid a)
deriving newtype instance FromJSON a => FromJSON (Semigroup.Dual a)
deriving newtype instance FromJSON a => FromJSON (Monoid.First a)
deriving newtype instance FromJSON a => FromJSON (Monoid.Last a)
deriving newtype instance FromJSON a => FromJSON (Identity a)
deriving newtype instance FromJSON a => FromJSON (Const a b)
deriving newtype instance FromJSON b => FromJSON (Tagged a b)

deriving anyclass instance (FromJSON (f a), FromJSON (g a), FromJSON a) => FromJSON (Sum f g a)
deriving anyclass instance (FromJSON a, FromJSON b) => FromJSON (Either a b)
deriving anyclass instance (FromJSON (f a), FromJSON (g a)) => FromJSON (Product f g a)
deriving anyclass instance (FromJSON a, FromJSON b) => FromJSON (a, b)
deriving anyclass instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (a, b, c)
deriving anyclass instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON (a, b, c, d)
deriving anyclass instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) => FromJSON (a, b, c, d, e)
deriving anyclass instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f) => FromJSON (a, b, c, d, e, f)
deriving anyclass instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g) => FromJSON (a, b, c, d, e, f, g)
