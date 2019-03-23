{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
module Scorch.Event.Type where

import Data.Int
import Data.Word

import Control.Lens.Wrapped
import Data.Vinyl.CoRec
import qualified SDL

fromSDLEvent :: MonadIO m => SDL.Event -> m (EventContext, Event)
fromSDLEvent (SDL.Event timestamp evt) = do
  mods <- getKeyModifiers
  case evt of
    SDL.KeyboardEvent SDL.KeyboardEventData{..} -> do
      pos <- getMousePos
      let
        eventMotion =
          if keyboardEventRepeat
          then KeyRepeat
          else fromSDLMotion keyboardEventMotion
        ctx = EventContext
          { eventOriginal = evt
          , eventTimestamp = timestamp
          , eventKeyModifiers = mods
          , eventPosition = pos
          , eventWindow = SDL.keyboardEventWindow
          }
        eventKey = Key keyboardEventKeysym
      pure (ctx, Event KeyMouseEvent{..})
    SDL.MouseButtonEvent SDL.MouseButtonEventData{..} -> do
      let
        eventMotion = fromSDLMotion mouseButtonEventMotion
        ctx = EventContext
          { eventOriginal = evt
          , eventTimestamp = timestamp
          , eventKeyModifiers = pos
          , eventWindow = SDL.mouseEventWindow
          }
        eventKey = Mouse mouseButtonEventWhich mouseButtonEventButton mouseButtonEventClicks
      pure (ctx, Event KeyMouseEvent{..})
    _ -> do


data EventContext = EventContext
  { eventOriginal :: SDL.EventPayload
  , eventTimestamp :: Word32
  , eventKeyModifiers :: KeyModifiers
  , eventPosition :: Position
  , eventWindow :: Maybe Window
  }
  deriving (Eq, Ord, Show, Generic)

type Position = SDL.Point (V2 Int32)

newtype Event = Event_ (CoRec Identity EventTypes)
  deriving (Eq, Ord, Show, Generic, Wrapped)

pattern Event :: NatToInt (RIndex e EventTypes) => e -> Event
pattern Event e <- (asA @e -> Just e)
  where Event e = CoRec (Identity e)

type EventTypes = '[KeyMouseEvent, OtherEvent]

instance (e ∈ EventTypes) => IsEventData Event e where
  _eventData = _Wrapped' . rprism

data KeyMouseEvent = KeyMouseEvent
  { eventKey :: KeyOrMouse
  , eventMotion :: KeyMotion
  }
  deriving (Eq, Ord, Show, Generic)

data KeyOrMouse
  = Key SDL.Keysym
  | Mouse SDL.MouseDevice SDL.MouseButton Word8
  deriving (Eq, Ord, Show, Generic)

data KeyMotion = KeyRepeat | KeyDown | KeyUp

fromSDLMotion :: SDL.InputMotion -> KeyMotion
fromSDLMotion SDL.Pressed = KeyDown
fromSDLMotion SDL.Released = KeyUp

data OtherEvent = OtherEvent
  deriving (Eq, Ord, Show, Generic)