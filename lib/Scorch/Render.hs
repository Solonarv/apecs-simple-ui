module Scorch.Render where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Int

import Apecs hiding (($=))
import Graphics.GL
import Linear.V2
import Linear.V4
import UnliftIO

import Apecs.Folds
import Scorch.Components

-- TODO use shaders

render :: (MonadIO m, Has w m Extent, Has w m Colored) => SystemT w m ()
render = do
  prims <- cfoldMap \(Extent pos dims, Colored (V4 r g b a)) -> do
    glColor4f r g b a
    rectTris pos dims
  liftIO do
    glClearColor 0.8 0.8 1 0
    glClear GL_COLOR_BUFFER_BIT
    glBegin GL_TRIANGLES
    prims
    glEnd

-- | Build a rectangle by specifying two triangles.
-- Meant to be used between 'glBegin' and 'glEnd'.
rectTris :: V2 Int32 -> V2 Int32 -> IO ()
rectTris (fmap fromIntegral -> V2 x y) (fmap fromIntegral -> V2 dx dy) = do
    vertex2 ul
    vertex2 bl
    vertex2 ur
    vertex2 ur
    vertex2 bl
    vertex2 br
  where
    bl = V2 x      y
    br = V2 (x+dx) y
    ul = V2 x      (y+dy)
    ur = V2 (x+dx) (y+dy)

vertex2 :: V2 Float -> IO ()
vertex2 (V2 x y) = glVertex2f x y