module Apecs.Folds where

import Apecs

cfoldMap :: forall c w m a. (Members w m c, Get w m c, Monoid a) => (c -> a) -> SystemT w m a
cfoldMap k = cfold (\a c -> a <> k c) mempty

cfoldMapM :: forall c w m a. (Members w m c, Get w m c, Monoid a) => (c -> SystemT w m a) -> SystemT w m a
cfoldMapM k = cfoldM (\a c -> (a <>) <$> k c) mempty