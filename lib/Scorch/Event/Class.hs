{-# LANGUAGE UndecidableInstances #-}
module Scorch.Event.Class where

import GHC.Generics (Generic)

import Control.Lens.Prism
import qualified Control.Lens.Unsound as Unsound
import SDL

import Util.Scorch.Optics.Iso
import Util.Scorch.TypeLevel.Disjoint

class IsEventData p e | e -> p where
  _eventData :: Prism' p e

instance (Disjoint Either e e', IsEventData p e, IsEventData p e') => IsEventData p (Either e e') where
  _eventData = Unsound.prismSum _eventData _eventData

-- * Unit data types for the events types that don't have an associated event data type
data KeymapChangedEventData = KeymapChangedEventData
  deriving (Show, Eq, Ord, Generic)
data QuitEventData = QuitEventData
  deriving (Show, Eq, Ord, Generic)
data ClipboardUpdateEventData = ClipboardUpdateEventData
  deriving (Show, Eq, Ord, Generic)

instance IsEventData SDL.EventPayload WindowShownEventData where
  _eventData = prism WindowShownEvent \case WindowShownEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowHiddenEventData where
  _eventData = prism WindowHiddenEvent \case WindowHiddenEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowExposedEventData where
  _eventData = prism WindowExposedEvent \case WindowExposedEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowMovedEventData where
  _eventData = prism WindowMovedEvent \case WindowMovedEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowResizedEventData where
  _eventData = prism WindowResizedEvent \case WindowResizedEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowSizeChangedEventData where
  _eventData = prism WindowSizeChangedEvent \case WindowSizeChangedEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowMinimizedEventData where
  _eventData = prism WindowMinimizedEvent \case WindowMinimizedEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowMaximizedEventData where
  _eventData = prism WindowMaximizedEvent \case WindowMaximizedEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowRestoredEventData where
  _eventData = prism WindowRestoredEvent \case WindowRestoredEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowGainedMouseFocusEventData where
  _eventData = prism WindowGainedMouseFocusEvent \case WindowGainedMouseFocusEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowLostMouseFocusEventData where
  _eventData = prism WindowLostMouseFocusEvent \case WindowLostMouseFocusEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowGainedKeyboardFocusEventData where
  _eventData = prism WindowGainedKeyboardFocusEvent \case WindowGainedKeyboardFocusEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowLostKeyboardFocusEventData where
  _eventData = prism WindowLostKeyboardFocusEvent \case WindowLostKeyboardFocusEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload WindowClosedEventData where
  _eventData = prism WindowClosedEvent \case WindowClosedEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload KeyboardEventData where
  _eventData = prism KeyboardEvent \case KeyboardEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload TextEditingEventData where
  _eventData = prism TextEditingEvent \case TextEditingEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload TextInputEventData where
  _eventData = prism TextInputEvent \case TextInputEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload KeymapChangedEventData where
  _eventData = prism (const KeymapChangedEvent) \case KeymapChangedEvent -> Right KeymapChangedEventData; e -> Left e
instance IsEventData SDL.EventPayload MouseMotionEventData where
  _eventData = prism MouseMotionEvent \case MouseMotionEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload MouseButtonEventData where
  _eventData = prism MouseButtonEvent \case MouseButtonEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload MouseWheelEventData where
  _eventData = prism MouseWheelEvent \case MouseWheelEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload JoyAxisEventData where
  _eventData = prism JoyAxisEvent \case JoyAxisEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload JoyBallEventData where
  _eventData = prism JoyBallEvent \case JoyBallEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload JoyHatEventData where
  _eventData = prism JoyHatEvent \case JoyHatEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload JoyButtonEventData where
  _eventData = prism JoyButtonEvent \case JoyButtonEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload JoyDeviceEventData where
  _eventData = prism JoyDeviceEvent \case JoyDeviceEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload ControllerAxisEventData where
  _eventData = prism ControllerAxisEvent \case ControllerAxisEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload ControllerButtonEventData where
  _eventData = prism ControllerButtonEvent \case ControllerButtonEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload ControllerDeviceEventData where
  _eventData = prism ControllerDeviceEvent \case ControllerDeviceEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload AudioDeviceEventData where
  _eventData = prism AudioDeviceEvent \case AudioDeviceEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload QuitEventData where
  _eventData = prism (const QuitEvent) \case QuitEvent -> Right QuitEventData; e -> Left e
instance IsEventData SDL.EventPayload UserEventData where
  _eventData = prism UserEvent \case UserEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload SysWMEventData where
  _eventData = prism SysWMEvent \case SysWMEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload TouchFingerEventData where
  _eventData = prism TouchFingerEvent \case TouchFingerEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload TouchFingerMotionEventData where
  _eventData = prism TouchFingerMotionEvent \case TouchFingerMotionEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload MultiGestureEventData where
  _eventData = prism MultiGestureEvent \case MultiGestureEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload DollarGestureEventData where
  _eventData = prism DollarGestureEvent \case DollarGestureEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload DropEventData where
  _eventData = prism DropEvent \case DropEvent d -> Right d; e -> Left e
instance IsEventData SDL.EventPayload ClipboardUpdateEventData where
  _eventData = prism (const ClipboardUpdateEvent) \case ClipboardUpdateEvent -> Right ClipboardUpdateEventData; e -> Left e
instance IsEventData SDL.EventPayload UnknownEventData where
  _eventData = prism UnknownEvent \case UnknownEvent d -> Right d; e -> Left e