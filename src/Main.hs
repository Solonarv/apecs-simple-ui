module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Int
import Data.Word

import Apecs hiding (($=))
import qualified Graphics.Rendering.OpenGL.GL as Gl
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.UI.GLFW as Glfw
import Linear.V2
import Linear.V4

import Scorch

main :: IO ()
main = do
  Glfw.init
  Glfw.windowHint $ Glfw.WindowHint'Resizable False
  Glfw.swapInterval 1
  Just win <- Glfw.createWindow 640 480 "Scorch Test" Nothing Nothing
  Glfw.makeContextCurrent (Just win)
  Gl.clearColor $= Gl.Color4 0 0 0.5 0
  Gl.shadeModel $= Gl.Smooth
  world <- initTestWorld
  runWith world setupTestScene
  let
    loop = do
      Glfw.pollEvents
      shouldClose <- Glfw.windowShouldClose win
      unless shouldClose do
        runWith world Scorch.render
        Glfw.swapBuffers win
        threadDelay 20_000
        loop
  loop

setupTestScene :: SystemT TestWorld IO ()
setupTestScene = do
  newEntity (Extent (V2 200 300) (V2 100 60), Colored (V4 1 0 0 0))
  newEntity (Extent (V2 170 320) (V2 80 200), Colored (V4 0 1 0 0))
  pure ()

data TestWorld = TestWorld
  { twEntityCounter :: Storage EntityCounter
  , twExtent        :: Storage Extent
  , twColored       :: Storage Colored
  }

instance Monad m => Has TestWorld m EntityCounter where
  getStore = asks twEntityCounter

instance Monad m => Has TestWorld m Extent where
  getStore = asks twExtent

instance Monad m => Has TestWorld m Colored where
  getStore = asks twColored

initTestWorld :: IO TestWorld
initTestWorld = TestWorld <$> explInit <*> explInit <*> explInit
