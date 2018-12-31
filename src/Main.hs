{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Int
import Data.Word
import Foreign
import Foreign.C

import Apecs
import Graphics.GL
import qualified SDL
import Linear.V2
import Linear.V4

import Scorch
import Shader
import TriangleShader

-- TODO Rewrite to use SDL for windowing

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Scorch Test" SDL.defaultWindow
    { SDL.windowOpenGL = Just SDL.defaultOpenGL
        { SDL.glColorPrecision = V4 8 8 8 8
        , SDL.glProfile        = SDL.Core SDL.Normal 3 2
        }
    , SDL.windowInitialSize = V2 640 480
    }
  ctx <- SDL.glCreateContext window
  SDL.glMakeCurrent window ctx
  prog <- initResources
  world <- initTestWorld
  runWith world setupTestScene
  let
    loop = do
      events <- fmap SDL.eventPayload <$> SDL.pollEvents
      whenEmpty [ () | SDL.WindowClosedEvent _ <- events ] do
        runWith world Scorch.render
        SDL.glSwapWindow window
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

whenEmpty :: (Foldable t, Applicative f) => t a -> f () -> f ()
whenEmpty = when . null

initResources :: IO Program
initResources = do
  -- VAO
  vao <- overPtr $ glGenVertexArrays 1
  glBindVertexArray vao
  -- VBO
  vbo <- overPtr $ glGenBuffers 1
  glBindBuffer GL_ARRAY_BUFFER vbo
  
  -- shader program
  Program program <- attachShaders =<< sequenceA [shVertex2D, shFragment2D]
  glUseProgram program

  -- Link Vertex data with Attributes
  posAttrib <- withCString "position" $ glGetAttribLocation program
  glVertexAttribPointer (fromIntegral posAttrib) 2 GL_FLOAT GL_FALSE 0 nullPtr
  glEnableVertexAttribArray (fromIntegral posAttrib)

  -- Uniforms
  uniColor <- withCString "triangleColor" $ glGetUniformLocation program
  glUniform3f uniColor 0 0 1

  pure (Program program)