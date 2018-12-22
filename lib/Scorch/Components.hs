module Scorch.Components where

import Data.Int

import Apecs
import Linear.V2
import Linear.V4

data Extent = Extent { extentCorner :: V2 Int32, extentDims :: V2 Int32 }
  deriving (Eq, Ord, Show)
instance Component Extent where type Storage Extent = Map Extent

newtype Colored = Colored (V4 Float)
  deriving (Eq, Ord, Show)
instance Component Colored where type Storage Colored = Map Colored

newtype ClickHandler m = ClickHandler (V2 Int32 -> m ())