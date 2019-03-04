{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UnliftedFFITypes   #-}

module Std.Data.JSON.Value where

import           Control.Monad
import           Data.Bits                ((.&.))
import           Data.Functor
import           Data.Primitive.PrimArray
import           Data.Scientific          (Scientific)
import           Data.Typeable
import           Data.Word
import           GHC.Generics
import           GHC.Stack
import qualified Std.Data.Parser          as P
import qualified Std.Data.Parser.Numeric  as P
import qualified Std.Data.Text            as T
import qualified Std.Data.Text.Base       as T
import           Std.Data.Vector.Base     as V
import           Std.Data.Vector.Extra    as V
import qualified Std.Data.Vector.FlatMap  as FM
import           Std.Foreign.PrimArray
import           System.IO.Unsafe         (unsafeDupablePerformIO)

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define COLON 58
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define C_0 48
#define C_9 57
#define C_A 65
#define C_F 70
#define C_a 97
#define C_f 102
#define C_n 110
#define C_t 116
#define MINUS    45

--------------------------------------------------------------------------------
-- | A JSON value represented as a Haskell value.
--
-- The 'Object''s payload is a key-value vector instead of a map, which parsed
-- directly from JSON document. This design choice has following advantages:
--
--    * Different strategy handling duplicated keys.
--    * Different 'Map' type to do further parsing, e.g. 'FM.FlatMap' from stdio.
--    * Roundtrip without touching the original key-value order.
--    * Save time if constructing map is not neccessary, e.g.
--      using a linear scan to find a key if only that key is needed.
--
-- There's no lazy parsing here, every pieces of JSON document will be parsed into a normal form
-- 'Value'. 'Object' and 'Array's payloads are packed into 'Vector's to avoid accumulating lists
-- in memory. Read more about <http://winterland.me/2019/03/05/aeson's-mysterious-lazy-parsing
-- why no lazy parsing is needed>.
--
data Value = Object {-# UNPACK #-} !(V.Vector (FM.TextKV Value))
           | Array  {-# UNPACK #-} !(V.Vector Value)
           | String {-# UNPACK #-} !T.Text
           | Number {-# UNPACK #-} !Scientific
           | Bool   !Bool
           | Null
         deriving (Eq, Show, Typeable, Generic)

parseValue :: HasCallStack => V.Bytes -> (V.Bytes, Either P.ParseError Value)
{-# INLINE parseValue #-}
parseValue = P.parse value

parseValue_ :: HasCallStack => V.Bytes -> Either P.ParseError Value
{-# INLINE parseValue_ #-}
parseValue_ = P.parse_ value

parseValueChunks :: (Monad m, HasCallStack) => m V.Bytes -> V.Bytes -> m (V.Bytes, Either P.ParseError Value)
{-# INLINE parseValueChunks #-}
parseValueChunks = P.parseChunks value

--------------------------------------------------------------------------------

-- | The only valid whitespace in a JSON document is space, newline,
-- carriage return, and tab.
skipSpaces :: Parser ()
skipSpaces = P.skipWhile $ \ w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
{-# INLINE skipSpaces #-}

-- | JSON 'Value' parser.
value :: HasCallStack => P.Parser Value
{-# INLINABLE value #-}
value = do
    skipSpaces
    w <- P.peek
    case w of
        DOUBLE_QUOTE    -> P.anyWord8 *> (String <$> string_)
        OPEN_CURLY      -> P.anyWord8 *> (Object <$> object_)
        OPEN_SQUARE     -> P.anyWord8 *> (Array <$> array_)
        C_f             -> P.bytes "false" $> (Bool False)
        C_t             -> P.bytes "true" $> (Bool True)
        C_n             -> P.bytes "null" $> Null
        _   | w >= 48 && w <= 57 || w == MINUS -> Number <$> P.scientific'
            | otherwise -> P.failWithStack "not a valid json value"

-- | parse json array with leading OPEN_SQUARE.
array :: HasCallStack => P.Parser (V.Vector Value)
{-# INLINE array #-}
array = P.word8 OPEN_SQUARE *> array_

-- | parse json array without leading OPEN_SQUARE.
array_ :: HasCallStack => P.Parser (V.Vector Value)
{-# INLINABLE array_ #-}
array_ = do
    skipSpaces
    w <- P.peek
    if w == CLOSE_SQUARE
    then P.anyWord8 $> V.empty
    else loop [] 1
  where
    loop :: [Value] -> Int -> P.Parser (V.Vector Value)
    loop acc !len = do
        !v <- value
        skipSpaces
        let acc' = v:acc
        ch <- P.satisfy $ \w -> w == COMMA || w == CLOSE_SQUARE
        if ch == COMMA
        then skipSpaces *> loop acc' (len+1)
        else return $! V.packRN len acc'

-- | parse json array with leading OPEN_CURLY.
object :: HasCallStack => P.Parser (V.Vector (FM.TextKV Value))
{-# INLINE object #-}
object = P.word8 OPEN_CURLY *> object_

-- | parse json object without leading OPEN_CURLY.
object_ :: HasCallStack => P.Parser (V.Vector (FM.TextKV Value))
{-# INLINABLE object_ #-}
object_ = do
    skipSpaces
    w <- P.peek
    if w == CLOSE_CURLY
    then P.anyWord8 $> V.empty
    else loop [] 1
 where
    loop :: [FM.TextKV Value] -> Int -> P.Parser (V.Vector (FM.TextKV Value))
    loop acc !n = do
        !k <- string
        skipSpaces
        P.word8 COLON
        !v <- value
        skipSpaces
        let acc' = FM.TextKV k v : acc
        ch <- P.satisfy $ \w -> w == COMMA || w == CLOSE_CURLY
        if ch == COMMA
        then skipSpaces *> loop acc' (n+1)
        else return $! V.packRN n acc'

--------------------------------------------------------------------------------

string :: HasCallStack => P.Parser T.Text
{-# INLINE string #-}
string = P.word8 DOUBLE_QUOTE *> string_

string_ :: HasCallStack => P.Parser T.Text
{-# INLINE string_ #-}
string_ = do
    (bs, state) <- P.scanChunks 0 go
    let mt = if state .&. 0xFF /= 0    -- need escaping
            then unsafeDupablePerformIO (do
                let !len = V.length bs
                !mpa <- newPrimArray len
                !len' <- withMutablePrimArrayUnsafe mpa (\ mba# _ ->
                    withPrimVectorUnsafe bs (decode_json_string mba#))
                !pa <- unsafeFreezePrimArray mpa
                if len' >= 0
                then return (Just (T.Text (V.PrimVector pa 0 len')))  -- unescaping also validate utf8
                else return Nothing)
            else (T.validateMaybe bs)
    case mt of
        Just t -> P.anyWord8 $> t
        _  -> P.failWithStack "utf8 validation or unescaping failed"
  where
    go :: Word32 -> V.Bytes -> Either Word32 (V.Bytes, V.Bytes, Word32)
    go !state v =
        case unsafeDupablePerformIO . withPrimUnsafe state $ \ ps ->
                withPrimVectorUnsafe v (find_json_string_end ps)
        of (state', len)
            | len >= 0 ->
                let !r = V.unsafeTake len v
                    !rest = V.unsafeDrop len v
                in Right (r, rest, state')
            | otherwise -> Left state'

foreign import ccall unsafe find_json_string_end :: MBA# Word32 -> BA# Word8 -> Int -> Int -> IO Int
foreign import ccall unsafe decode_json_string :: MBA# Word8 -> BA# Word8 -> Int -> Int -> IO Int
