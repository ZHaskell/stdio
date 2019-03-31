module Std.Data.Instance where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances.UnorderedContainers
import Test.QuickCheck.Instances.Scientific

import qualified Std.Data.Vector as V
import qualified Std.Data.Text as T
import qualified Std.Data.Vector.FlatMap as FM
import qualified Std.Data.JSON.Value as JSON

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = V.pack <$> arbitrary
    shrink a = V.pack <$> (shrink (V.unpack a))

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary
    shrink a = T.pack <$> (shrink (T.unpack a))

instance Arbitrary a => Arbitrary (FM.TextKV a) where
    arbitrary = do
        k <- arbitrary
        v <- arbitrary
        return (FM.TextKV k v)
    shrink (FM.TextKV k v) = [ FM.TextKV k' v' | k' <- shrink k, v' <- shrink v]

instance Arbitrary JSON.Value where
    arbitrary = sized (arbitraryValue 0)
      where
        arbitraryValue d s = do
            i <- arbitrary :: Gen Word
            case (i `mod` 6) of
                0 -> if d < s then JSON.Object . V.pack <$> listOf (arbitraryFM (d+1) s)
                              else return JSON.Null
                1 -> if d < s then JSON.Array . V.pack <$> listOf (arbitraryValue (d+1) s)
                              else return JSON.Null
                2 -> JSON.String <$> arbitrary
                3 -> JSON.Number <$> arbitrary
                4 -> JSON.Bool <$> arbitrary
                _ -> return JSON.Null

        arbitraryFM d s = do
            k <- arbitrary
            v <- arbitraryValue d s
            return (FM.TextKV k v)

    shrink (JSON.Object kvs) = FM.val <$> (V.unpack kvs)
    shrink (JSON.Array vs) = V.unpack vs
    shrink _          = []
