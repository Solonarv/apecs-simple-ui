module Scorch.Components where

import Data.Word
import Foreign.C.Types

import Apecs
import Linear.V2
import Linear.V4

data Extent = Extent { extentCorner :: V2 CInt, extentDims :: V2 CInt }
  deriving (Eq, Ord, Show)
instance Component Extent where type Storage Extent = Map Extent

newtype Colored = Colored (V4 Word8)
  deriving (Eq, Ord, Show)
instance Component Colored where type Storage Colored = Map Colored

newtype ClickHandler w m = ClickHandler (V2 CInt -> SystemT w m ())

data RenderCallbacks w m = RenderCallbacks
  { renderCallbackPre :: SystemT w m ()
  , renderCallbackPost :: SystemT w m ()
  }

instance Applicative m => Semigroup (RenderCallbacks w m) where
  RenderCallbacks pre1 post1 <> RenderCallbacks pre2 post2 = RenderCallbacks (pre1 *> pre2) (post1 *> post2)

instance Applicative m => Monoid (RenderCallbacks w m) where
  mempty = RenderCallbacks (pure ()) (pure ())

instance Applicative m => Component (RenderCallbacks w m) where
  type Storage (RenderCallbacks w m) = Global (RenderCallbacks w m)