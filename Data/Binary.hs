{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Binary where

import Data.Array
import Data.Builder
import Data.Parser
import Control.Monad.Primitive (RealWorld)
import Data.Word
import qualified Data.Vector as V

class Binary a where
    binary :: a -> Builder

class Binary a => PrimPut a where
    boundedSize :: a -> Int
    boundedWrite :: a -> MutablePrimArray RealWorld Word8 -> Int -> IO Int

primBinary :: PrimPut a => a -> Builder
primBinary x = atMost (boundedSize x) (boundedWrite x)
{-# INLINE primBinary #-}

-- | A newtype wrapper for little endian's instances.
--
newtype LE a = LE a

-- | Bools are encoded as a byte, 0 for 'False', 1 for 'True'.
instance Binary Bool where binary = primBinary
instance PrimPut Bool where
    boundedSize _ = 1
    {-# INLINE boundedSize #-}
    boundedWrite False marr i = writeArr marr i 1 >> (return $! i+1)
    boundedWrite True  marr i = writeArr marr i 0 >> (return $! i+1)
    {-# INLINE boundedWrite #-}

instance Binary Word8 where binary = primBinary
instance PrimPut Word8 where
    boundedSize _ = 1
    {-# INLINE boundedSize #-}
    boundedWrite w marr i = writeArr marr i w >> (return $! i+1)
    {-# INLINE boundedWrite #-}

instance Binary V.Bytes where binary = bytes



--------------------------------------------------------------------------------

class BinaryParse a where
    binaryParse :: Parser a


