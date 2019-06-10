{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


module Std.Data.Generics.Utils
  ( ProductSize(..)
  , productSize
  ) where

import GHC.Generics
import GHC.TypeNats
import GHC.Exts (Proxy#, proxy#)

-- | type class for calculating product size.
class KnownNat (PSize f) => ProductSize (f :: * -> *) where
    type PSize f :: Nat

instance ProductSize (S1 s a) where
    type PSize (S1 s a) = 1
instance (KnownNat (PSize a + PSize b), ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    type PSize (a :*: b) = PSize a + PSize b

productSize :: forall f. KnownNat (PSize f) => Proxy# f -> Int
productSize _ = fromIntegral (natVal' (proxy# :: Proxy# (PSize f)))
