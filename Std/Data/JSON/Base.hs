{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

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
import           Control.Monad
import           Control.Monad.ST
import qualified Data.HashMap.Strict     as HM
import qualified Std.Data.Vector.FlatMap as FM
import qualified Std.Data.Vector.Base    as V
import qualified Std.Data.Vector.Extra   as V
import qualified Std.Data.Text           as T
import qualified Control.Monad.Fail      as Fail
import           Data.Scientific         (Scientific, toBoundedInteger)
import qualified Std.Data.Builder        as B
import qualified Std.Data.TextBuilder    as TB
import qualified Std.Data.Parser         as P
import           Std.Data.Generics.Utils
import           Std.Data.JSON.Value
import qualified Std.Data.JSON.Value.Builder as JB
import qualified Data.Primitive.SmallArray as A
import Data.Data
import Data.Word
import Data.Typeable
import GHC.Generics
import GHC.TypeNats
import GHC.Exts (proxy#, Proxy#)

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define COLON 58
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91

--------------------------------------------------------------------------------

parse :: FromJSON a => V.Bytes -> Result a
parse = undefined

parseChunks :: (FromJSON a, Monad m) => m V.Bytes -> V.Bytes -> m (V.Bytes, Result a)
parseChunks = undefined

--------------------------------------------------------------------------------

-- | Elements of a JSON path used to describe the location of an error.
data PathElement
    = Key {-# UNPACK #-} !T.Text
        -- ^ JSON path element of a key into an object,
        -- \"object.key\".
    | Index {-# UNPACK #-} !Int
        -- ^ JSON path element of an index into an
        -- array, \"array[index]\".
    | EmbeddedJSON
        -- ^ JSON path of a embedded JSON String
  deriving (Eq, Show, Typeable, Ord)

type Path = [PathElement]

data Result r
    = Success r
    | Failure Path T.Text

instance Show r => Show (Result r) where
    -- TODO use standard format
    show (Success r) = "Success " ++ show r
    show (Failure paths msg) = T.unpack . TB.buildText $ do
        "Failure <"
        mapM_ renderPath (reverse paths)
        "> "
        TB.text msg
      where
        renderPath (Index ix) = TB.char7 '[' >> TB.int ix >> TB.char7 ']'
        renderPath (Key k) = TB.char7 '.' >> TB.text k
        renderPath EmbeddedJSON = "<EmbeddedJSON>"


-- | 'Parser' for parsing result from JSON 'Value'.
newtype Parser a = Parser { runParser :: forall r. (Path -> T.Text -> r) -> (a -> r) -> r }

-- | Run a 'Parser' with input value.
parseEither :: (a -> Parser r) -> a -> Result r
{-# INLINE parseEither #-}
parseEither m v = runParser (m v) Failure Success

instance Functor Parser where
    fmap f m = Parser (\ kf k -> runParser m kf (k . f))
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure a = Parser (\ _ k -> k a)
    {-# INLINE pure #-}
    (Parser f) <*> (Parser g) = Parser (\ kf k ->
        f kf (\ f' ->  g kf (k . f')))
    {-# INLINE (<*>) #-}

instance Alternative Parser where
    {-# INLINE (<|>) #-}
    (Parser f) <|> (Parser g) = Parser (\ kf k -> f (\ _ _ -> g kf k) k)
    {-# INLINE empty #-}
    empty = fail' "Std.Data.JSON.Base(Alternative).empty"

instance MonadPlus Parser where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance Monad Parser where
    (Parser f) >>= g = Parser (\ kf k ->
        f kf (\ a -> runParser (g a) kf k))
    {-# INLINE (>>=) #-}
    return = pure
    {-# INLINE return #-}
    fail = Fail.fail
    {-# INLINE fail #-}

instance Fail.MonadFail Parser where
    {-# INLINE fail #-}
    fail = fail' . T.pack

-- | 'T.Text' version of 'fail'.
fail' :: T.Text -> Parser a
{-# INLINE fail' #-}
fail' msg = Parser (\ kf _ -> kf [] msg)

--------------------------------------------------------------------------------

typeMismatch :: T.Text     -- ^ The name of the type you are trying to parse.
             -> Value      -- ^ The actual value encountered.
             -> Parser a
typeMismatch expected actual =
    fail' $ T.concat ["expected ", expected, ", encountered ", name]
  where
    name = case actual of
        Object _ -> "Object"
        Array _  -> "Array"
        String _ -> "String"
        Number _ -> "Number"
        Bool _   -> "Boolean"
        Null     -> "Null"


-- | Add JSON Path context to a parser
--
-- When parsing a complex structure, it helps to annotate (sub)parsers
-- with context, so that if an error occurs, you can find its location.
--
-- > withObject "Person" $ \o ->
-- >   Person
-- >     <$> o .: "name" <?> Key "name"
-- >     <*> o .: "age" <?> Key "age"
--
-- (Standard methods like '(.:)' already do this.)
--
-- With such annotations, if an error occurs, you will get a JSON Path
-- location of that error.
(<?>) :: Parser a -> PathElement -> Parser a
{-# INLINE (<?>) #-}
(Parser p) <?> path = Parser (\ kf k -> p (kf . (path:)) k)
infixl 9 <?>

-- | Add context to a failure message, indicating the name of the structure
-- being parsed.
--
-- > prependContext "MyType" (fail "[error message]")
-- > -- Error: "parsing MyType failed, [error message]"
prependContext :: T.Text -> Parser a -> Parser a
{-# INLINE prependContext #-}
prependContext name (Parser p) = Parser (\ kf k ->
    p (\ paths msg -> kf paths (T.concat ["parsing ", name, " failed, ", msg])) k)

withBool :: (Bool -> Parser a) -> Value ->  Parser a
withBool f (Bool x)  = f x
withBool f v         = typeMismatch "Bool" v

withNumber :: (Scientific -> Parser a) -> Value ->  Parser a
withNumber f (Number x)  = f x
withNumber f v           = typeMismatch "Number" v

withString :: (T.Text -> Parser a) -> Value -> Parser a
withString f (String x)  = f x
withString f v           = typeMismatch "String" v

withArray :: (V.Vector Value -> Parser a) -> Value -> Parser a
withArray f (Array arr)  = f arr
withArray f v            = typeMismatch "Array" v

-- | Directly use 'Object' as key-values for further parsing.
withKeyValues :: (V.Vector (T.Text, Value) -> Parser a) -> Value -> Parser a
withKeyValues f (Object kvs) = f kvs
withKeyValues f v            = typeMismatch "Object" v

-- | Take a 'Object' as an 'FM.FlatMap T.Text Value', on key duplication prefer first one.
withFlatMap :: (FM.FlatMap T.Text Value -> Parser a) -> Value -> Parser a
withFlatMap f (Object obj) = f (FM.packVector obj)
withFlatMap f v            = typeMismatch "Object" v

-- | Take a 'Object' as an 'FM.FlatMap T.Text Value', on key duplication prefer last one.
withFlatMapR :: (FM.FlatMap T.Text Value -> Parser a) -> Value -> Parser a
withFlatMapR f (Object obj) = f (FM.packVectorR obj)
withFlatMapR f v            = typeMismatch "Object" v

-- | Take a 'Object' as an 'HM.HashMap T.Text Value', on key duplication prefer first one.
withHashMap :: (HM.HashMap T.Text Value -> Parser a) -> Value -> Parser a
withHashMap f (Object obj) = f (HM.fromList (V.unpackR obj))
withHashMap f v            = typeMismatch "Object" v

-- | Take a 'Object' as an 'HM.HashMap T.Text Value', on key duplication prefer last one.
withHashMapR :: (HM.HashMap T.Text Value -> Parser a) -> Value -> Parser a
withHashMapR f (Object obj) = f (HM.fromList (V.unpack obj))
withHashMapR f v            = typeMismatch "Object" v

-- | Decode a nested JSON-encoded string.
withEmbeddedJSON :: (Value -> Parser a)     -- ^ a inner parser which will get the parsed 'Value'.
                 -> Value -> Parser a       -- a parser take a JSON String
withEmbeddedJSON innerParser (String txt) = Parser (\ kf k ->
        case parse (T.getUTF8Bytes txt) of
            Success v -> runParser (innerParser v) (\ paths msg -> kf (EmbeddedJSON:paths) msg) k
            Failure paths msg -> kf paths msg)
  where
withEmbeddedJSON _ v = typeMismatch "String" v
{-# INLINE withEmbeddedJSON #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '.:?' instead.
(.:) :: (FromJSON a) => FM.FlatMap T.Text Value -> T.Text -> Parser a
{-# INLINE (.:) #-}
(.:) = parseField fromJSON

-- | Retrieve the value associated with the given key of an 'Object'. The
-- result is 'Nothing' if the key is not present or if its value is 'Null',
-- or 'empty' if the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
(.:?) :: (FromJSON a) => FM.FlatMap T.Text Value -> T.Text -> Parser (Maybe a)
{-# INLINE (.:?) #-}
(.:?) = parseFieldMaybe fromJSON

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present or 'empty' if the
-- value cannot be converted to the desired type.
--
-- This differs from '.:?' by attempting to parse 'Null' the same as any
-- other JSON value, instead of interpreting it as 'Nothing'.
(.:!) :: (FromJSON a) => FM.FlatMap T.Text Value -> T.Text -> Parser (Maybe a)
{-# INLINE (.:!) #-}
(.:!) = parseFieldMaybe' fromJSON

parseField :: (Value -> Parser a)  -- ^ the field parser (value part of a key value pair)
           -> FM.FlatMap T.Text Value -> T.Text -> Parser a
parseField p obj key = case FM.lookup key obj of
    Just v -> p v <?> Key key
    _      -> fail (T.unpack . T.concat $ ["key ", key, " not present"])

-- | Variant of '.:?' with explicit parser function.
parseFieldMaybe :: (Value -> Parser a) -> FM.FlatMap T.Text Value -> T.Text -> Parser (Maybe a)
{-# INLINE parseFieldMaybe #-}
parseFieldMaybe p obj key = case FM.lookup key obj of
    Just Null -> pure Nothing
    Just v    -> Just <$> p v <?> Key key
    _         -> pure Nothing

-- | Variant of '.:!' with explicitliftParseJSON p (listParser p) parser function.
parseFieldMaybe' :: (Value -> Parser a) -> FM.FlatMap T.Text Value -> T.Text -> Parser (Maybe a)
{-# INLINE parseFieldMaybe' #-}
parseFieldMaybe' p obj key = case FM.lookup key obj of
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

class ToJSON a where
    toJSON :: a -> Value
    default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
    toJSON = gToJSON defaultSetting . from

instance ToJSON Int where
    toJSON = Number . fromIntegral

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

instance (ProductSize a, KnownNat (PSize a), GWriteFields a, GWriteFields b, Field a ~ Field b) => GWriteFields (a :*: b) where
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
        return (Array (V.Vector arr 0 l))

instance GMergeFields (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gMergeFields #-}
    gMergeFields _ marr = do
        arr <- A.unsafeFreezeSmallArray marr
        let l = A.sizeofSmallArray arr
        return (Object (V.Vector arr 0 l))

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
instance (ProductSize (a :*: b), KnownNat (PSize (a :*: b)), GWriteFields (a :*: b), GMergeFields (a :*: b), Constructor c)
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

encodeJSON' :: EncodeJSON a => a -> TB.TextBuilder ()
{-# INLINE encodeJSON' #-}
encodeJSON' = TB.TextBuilder . encodeJSON

instance EncodeJSON Int where
    {-# INLINE encodeJSON #-}
    encodeJSON = B.int

instance EncodeJSON Char where
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.string . T.singleton

instance EncodeJSON T.Text where
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.string

instance {-# OVERLAPPING #-} EncodeJSON String where
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.string . T.pack

instance {-# OVERLAPPABLE #-} EncodeJSON a => EncodeJSON [a] where
    {-# INLINE encodeJSON #-}
    encodeJSON xs = do
        B.encodePrim @Word8 OPEN_SQUARE
        go xs
        B.encodePrim @Word8 CLOSE_SQUARE
      where
        go [] = return ()
        go [x] = encodeJSON x
        go (x:xs) = encodeJSON x >> B.encodePrim @Word8 COMMA >> go xs


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
    fromJSON :: Value -> Parser a
    default fromJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
    fromJSON v = to <$> gFromJSON defaultSetting v

parseBoundedIntegralFromScientific :: (Bounded a, Integral a) => Scientific -> Parser a
{-# INLINE parseBoundedIntegralFromScientific #-}
parseBoundedIntegralFromScientific s = maybe
    (fail $ "value is either floating or will cause over or underflow " ++ show s)
    pure
    (toBoundedInteger s)

parseBoundedIntegral :: (Bounded a, Integral a) => T.Text -> Value -> Parser a
{-# INLINE parseBoundedIntegral #-}
parseBoundedIntegral name =
    prependContext name . withNumber parseBoundedIntegralFromScientific

instance FromJSON Int where
    fromJSON = parseBoundedIntegral "Int"

instance FromJSON Value where
    fromJSON = return

instance FromJSON a => FromJSON (V.Vector (T.Text, a)) where
    {-# INLINE fromJSON #-}
    fromJSON (Object kvs) = mapM (\ (k,v) -> fromJSON v >>= \ !v' -> return (k, v')) kvs
    fromJSON v            = typeMismatch "Object" v

-- | default instance prefer later key
instance FromJSON a => FromJSON (FM.FlatMap T.Text a) where
    {-# INLINE fromJSON #-}
    fromJSON = withFlatMapR (mapM fromJSON)

-- | default instance prefer later key
instance FromJSON a => FromJSON (HM.HashMap T.Text a) where
    {-# INLINE fromJSON #-}
    fromJSON = withHashMapR (mapM fromJSON)

instance FromJSON a => FromJSON (V.Vector a) where
    {-# INLINE fromJSON #-}
    fromJSON = withArray (mapM fromJSON)

class GFromJSON f where
    gFromJSON :: Settings -> Value -> Parser (f a)

--------------------------------------------------------------------------------
-- Selectors

type family LookupTable f where
    LookupTable (a :*: b) = LookupTable a
    LookupTable (S1 (MetaSel Nothing u ss ds) f) = V.Vector Value
    LookupTable (S1 (MetaSel (Just l) u ss ds) f) = FM.FlatMap T.Text Value

class GFromFields f where
    gFromFields :: Settings -> LookupTable f -> Int -> Parser (f a)

instance (ProductSize a, KnownNat (PSize a), GFromFields a, GFromFields b, LookupTable a ~ LookupTable b)
    => GFromFields (a :*: b) where
    {-# INLINE gFromFields #-}
    gFromFields s v idx = do
        !a <- gFromFields s v idx
        !b <- gFromFields s v (idx + productSize (proxy# :: Proxy# a))
        return (a :*: b)

instance (GFromJSON f) => GFromFields (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFromFields #-}
    gFromFields s v idx = do
        !v' <- V.unsafeIndexM v idx
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

instance FromJSON a => GFromJSON (K1 i a) where
    {-# INLINE gFromJSON #-}
    gFromJSON s x = K1 <$> fromJSON x

class GBuildLookup f where
    gBuildLookup :: Proxy# f -> Value -> Parser (LookupTable f)

instance (GBuildLookup a, GBuildLookup b) => GBuildLookup (a :*: b) where
    gBuildLookup _ x = gBuildLookup (proxy# :: Proxy# a) x

instance GBuildLookup (S1 (MetaSel Nothing u ss ds) f) where
    gBuildLookup _ (Array v) = return v
    gBuildLookup _ x         = typeMismatch "Array" x

instance GBuildLookup (S1 ((MetaSel (Just l) u ss ds)) f) where
    gBuildLookup _ (Object v) = return $! FM.packVectorR v
    gBuildLookup _ x         = typeMismatch "Object" x

--------------------------------------------------------------------------------
-- Constructors

class GConstrFromJSON f where
    gConstrFromJSON :: Bool -> Settings -> Value -> Parser (f a)

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
        | cn == x   = return (M1 U1)
        | otherwise = fail' ("Std.Data.JSON.Base: unknown constructor name " <> x)
      where cn = constrFmt s $ conName (undefined :: t c U1 a)
    gConstrFromJSON _ _ v = typeMismatch "String" v

-- | Constructor with a single payload
instance (Constructor c, GFromJSON (S1 sc f)) => GConstrFromJSON (C1 c (S1 sc f)) where
    {-# INLINE gConstrFromJSON #-}
    gConstrFromJSON False s x = M1 <$> gFromJSON s x
    gConstrFromJSON True s x = case x of
        Object v -> case V.indexM v 0 of
            Just (k, v') | k == cn -> M1 <$> gFromJSON s v' <?> Key cn
            _                      -> fail' ("Std.Data.JSON.Base: constructor not found" <> cn)
        _ ->  typeMismatch "Object" x
      where cn = constrFmt s $ conName (undefined :: t c f a)

-- | Constructor with multiple payloads
instance (GFromFields (a :*: b), GBuildLookup (a :*: b), Constructor c)
    => GConstrFromJSON (C1 c (a :*: b)) where
    {-# INLINE gConstrFromJSON #-}
    gConstrFromJSON False s x = do
        t <- gBuildLookup (proxy# :: Proxy# (a :*: b)) x
        M1 <$> gFromFields s t 0
    gConstrFromJSON True s x = case x of
        Object v -> case V.indexM v 0 of
            Just (k, v') | k == cn -> do t <- gBuildLookup (proxy# :: Proxy# (a :*: b)) v'
                                         M1 <$> gFromFields s t 0
            _                      -> fail' ("Std.Data.JSON.Base: constructor not found" <> cn)
        _ ->  typeMismatch "Object" x
      where cn = constrFmt s $ conName (undefined :: t c f a)

--------------------------------------------------------------------------------
-- Data types
instance GConstrFromJSON f => GFromJSON (D1 c f) where
    {-# INLINE gFromJSON #-}
    gFromJSON s x = M1 <$> gConstrFromJSON False s x
