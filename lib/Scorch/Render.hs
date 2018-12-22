module Scorch.Render where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Int

import Apecs hiding (($=))
import qualified Graphics.Rendering.OpenGL.GL as Gl
import           Graphics.Rendering.OpenGL.GL (($=))
import Linear.V2
import Linear.V4
import UnliftIO

import Apecs.Folds
import Scorch.Components

render :: (MonadIO m, Has w m Extent, Has w m Colored) => SystemT w m ()
render = do
  prims <- cfoldMap \(Extent pos dims, Colored (V4 r g b a)) -> do
    Gl.color (Gl.Color4 r g b a)
    quad pos dims
  liftIO do
    Gl.clear [Gl.ColorBuffer]
    prims

quad :: V2 Int32 -> V2 Int32 -> IO ()
quad (V2 x y) (V2 dx dy) = Gl.renderPrimitive Gl.Polygon do
  let
    ul = Gl.Vertex2 x      y
    ur = Gl.Vertex2 (x+dx) y
    bl = Gl.Vertex2 x      (y+dy)
    br = Gl.Vertex2 (x+dx) (y+dy)
  Gl.vertex ul
  Gl.vertex ur
  Gl.vertex br
  Gl.vertex bl