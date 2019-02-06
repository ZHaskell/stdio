{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Std.Data.PrimArray.UnalignedAccessSpec where

import qualified Data.List                as L
import           Data.Int
import           GHC.Float
import           Data.Word
import           Std.Data.PrimArray.UnalignedAccess
import           Control.Monad.Primitive
import           Data.Primitive.ByteArray
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "unaligned acess" . modifyMaxSuccess (*10) . modifyMaxSize (*10)  $ do
        prop "roundtrip Word8" $ \ (w::Word8) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# w)
            w' <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let w'' = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip Word16" $ \ (w::Word16) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# w)
            w' <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let w'' = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip Word32" $ \ (w::Word32) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# w)
            w' <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let w'' = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip Word64" $ \ (w::Word64) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# w)
            w' <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let w'' = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip Word" $ \ (w::Word) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# w)
            w' <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let w'' = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip LE Word16" $ \ (w::Word16) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (LE w))
            (LE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (LE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip LE Word32" $ \ (w::Word32) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (LE w))
            (LE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (LE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip LE Word64" $ \ (w::Word64) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (LE w))
            (LE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (LE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip LE Word" $ \ (w::Word) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (LE w))
            (LE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (LE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip BE Word16" $ \ (w::Word16) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (BE w))
            (BE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (BE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip BE Word32" $ \ (w::Word32) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (BE w))
            (BE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (BE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip BE Word64" $ \ (w::Word64) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (BE w))
            (BE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (BE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip BE Word" $ \ (w::Word) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (BE w))
            (BE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (BE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

--------------------------------------------------------------------------------

        prop "roundtrip Int16" $ \ (w::Int16) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# w)
            w' <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let w'' = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip LE Int16" $ \ (w::Int16) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (LE w))
            (LE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (LE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip BE Int16" $ \ (w::Int16) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (BE w))
            (BE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (BE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip Int32" $ \ (w::Int32) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# w)
            w' <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let w'' = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip LE Int32" $ \ (w::Int32) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (LE w))
            (LE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (LE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip BE Int32" $ \ (w::Int32) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (BE w))
            (BE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (BE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip Int64" $ \ (w::Int64) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# w)
            w' <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let w'' = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip LE Int64" $ \ (w::Int64) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (LE w))
            (LE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (LE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip BE Int64" $ \ (w::Int64) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (BE w))
            (BE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (BE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip Int" $ \ (w::Int) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# w)
            w' <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let w'' = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip LE Int" $ \ (w::Int) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (LE w))
            (LE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (LE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip BE Int" $ \ (w::Int) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (BE w))
            (BE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (BE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip Float" $ \ (w::Float) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# w)
            w' <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let w'' = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip LE Float" $ \ (w::Float) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (LE w))
            (LE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (LE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip BE Float" $ \ (w::Float) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (BE w))
            (BE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (BE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip Double" $ \ (w::Double) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# w)
            w' <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let w'' = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip LE Double" $ \ (w::Double) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (LE w))
            (LE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (LE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')

        prop "roundtrip BE Double" $ \ (w::Double) -> ioProperty $ do
            mba@(MutableByteArray mba#) <- newByteArray 128
            primitive_ (writeWord8ArrayAs mba# 7# (BE w))
            (BE w') <- primitive (readWord8ArrayAs mba# 7#)
            (ByteArray ba#) <- unsafeFreezeByteArray mba
            let (BE w'') = indexWord8ArrayAs ba# 7#
            return $ (w === w') .&&. (w === w'')
