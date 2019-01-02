module Scorch.Render where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Int

import Apecs hiding (($=))
import Linear.V2
import Linear.V4
import SDL hiding (get)
import UnliftIO

import Apecs.Folds
import Scorch.Components

-- TODO use shaders

render :: (MonadIO m, Has w m Extent, Has w m Colored, Has w m (RenderCallbacks w m)) => SDL.Renderer -> SystemT w m ()
render rdr = do
  renderCallbackPre =<< get global
  renderBoxes rdr
  renderCallbackPost =<< get global

renderBoxes :: (MonadIO m, Has w m Extent, Has w m Colored) => SDL.Renderer -> SystemT w m ()
renderBoxes rdr = do
  rendererDrawColor rdr $= V4 127 255 255 255
  clear rdr
  cmapM_ \(Extent pos dims, Colored color) -> do
    rendererDrawColor rdr $= color
    fillRect rdr (Just (rect pos dims))
  present rdr

rect :: V2 a -> V2 a -> SDL.Rectangle a
rect pos dims = Rectangle (P pos) dims