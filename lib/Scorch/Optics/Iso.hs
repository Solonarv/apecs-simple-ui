{-# LANGUAGE UndecidableInstances #-}
module Scorch.Optics.Iso
  ( gRep
  , IsUnit(..)
  , pattern Unit
  , module Control.Lens.Iso
  ) where

import GHC.Generics as Generic

import Control.Lens.Getter
import Control.Lens.Iso
import Control.Lens.Review

gRep :: (Generic s, Generic t) => Iso s t (Rep s x) (Rep t x)
gRep = iso Generic.from Generic.to

class IsUnit t where
  _Unit :: Iso' t ()

pattern Unit :: IsUnit t => t
pattern Unit <- (view _Unit -> ())
  where Unit = _Unit # ()

instance (Generic t, GIsUnit (Rep t)) => IsUnit t where
  _Unit = gRep . _GUnit

class GIsUnit f where
  _GUnit :: Iso' (f x) ()

instance GIsUnit U1 where
  _GUnit = iso (const ()) (const U1)

instance (GIsUnit f, GIsUnit g) => GIsUnit (f :*: g) where
  _GUnit = iso (const ()) (const $ _GUnit # () :*: _GUnit # ())

instance IsUnit c => GIsUnit (K1 i c) where
  _GUnit = iso unK1 K1 . _Unit @c

instance GIsUnit f => GIsUnit (M1 i t f) where
  _GUnit = iso unM1 M1 . _GUnit @f