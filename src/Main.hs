{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Int
import Data.Word
import Foreign
import Foreign.C

import Apecs
import SDL
import Linear.V2
import Linear.V4

import Scorch

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Scorch Test" SDL.defaultWindow
    { SDL.windowInitialSize = V2 640 480
    }
  rdr <- SDL.createRenderer window 0 defaultRenderer
    { SDL.rendererType = SDL.AcceleratedVSyncRenderer
    , SDL.rendererTargetTexture = True
    }
  world <- initTestWorld
  runWith world setupTestScene
  let
    loop = do
      events <- fmap SDL.eventPayload <$> SDL.pollEvents
      whenEmpty [ () | SDL.WindowClosedEvent _ <- events ] do
        runWith world (Scorch.render rdr)
        threadDelay 20_000
        loop
  loop

setupTestScene :: SystemT TestWorld IO ()
setupTestScene = do
  newEntity (Extent (V2 200 300) (V2 100 60), Colored (V4 255 0 0 255))
  newEntity (Extent (V2 170 320) (V2 80 200), Colored (V4 0 255 0 255))
  pure ()

data TestWorld = TestWorld
  { twEntityCounter   :: Storage EntityCounter
  , twExtent          :: Storage Extent
  , twColored         :: Storage Colored
  , twRenderCallbacks :: Storage (RenderCallbacks TestWorld IO)
  }

instance Monad m => Has TestWorld m EntityCounter where
  getStore = asks twEntityCounter

instance Monad m => Has TestWorld m Extent where
  getStore = asks twExtent

instance Monad m => Has TestWorld m Colored where
  getStore = asks twColored

instance Monad m => Has TestWorld m (RenderCallbacks TestWorld IO) where
  getStore = asks twRenderCallbacks

initTestWorld :: IO TestWorld
initTestWorld = TestWorld <$> explInit <*> explInit <*> explInit <*> explInit

whenEmpty :: (Foldable t, Applicative f) => t a -> f () -> f ()
whenEmpty = when . null