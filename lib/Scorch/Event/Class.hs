{-# LANGUAGE UndecidableInstances #-}
module Scorch.Event.Class where

import GHC.Generics (Generic)

import Control.Lens.Prism
import qualified Control.Lens.Unsound as Unsound
import Data.Generics.Sum.Constructors
import SDL

import Scorch.Optics.Iso
import Scorch.TypeLevel.Disjoint

class IsEventData e where
  _eventData :: Prism' SDL.EventPayload e

instance (Disjoint Either e e', IsEventData e, IsEventData e') => IsEventData (Either e e') where
  _eventData = Unsound.prismSum _eventData _eventData

-- * Unit data types for the events types that don't have an associated event data type
data KeymapChangedEventData = KeymapChangedEventData
  deriving (Show, Eq, Ord, Generic)
data QuitEventData = QuitEventData
  deriving (Show, Eq, Ord, Generic)
data ClipboardUpdateEventData = ClipboardUpdateEventData
  deriving (Show, Eq, Ord, Generic)

instance IsEventData WindowShownEventData where _eventData = _Ctor @"WindowShownEvent"
instance IsEventData WindowHiddenEventData where _eventData = _Ctor @"WindowHiddenEvent"
instance IsEventData WindowExposedEventData where _eventData = _Ctor @"WindowExposedEvent"
instance IsEventData WindowMovedEventData where _eventData = _Ctor @"WindowMovedEvent"
instance IsEventData WindowResizedEventData where _eventData = _Ctor @"WindowResizedEvent"
instance IsEventData WindowSizeChangedEventData where _eventData = _Ctor @"WindowSizeChangedEvent"
instance IsEventData WindowMinimizedEventData where _eventData = _Ctor @"WindowMinimizedEvent"
instance IsEventData WindowMaximizedEventData where _eventData = _Ctor @"WindowMaximizedEvent"
instance IsEventData WindowRestoredEventData where _eventData = _Ctor @"WindowRestoredEvent"
instance IsEventData WindowGainedMouseFocusEventData where _eventData = _Ctor @"WindowGainedMouseFocusEvent"
instance IsEventData WindowLostMouseFocusEventData where _eventData = _Ctor @"WindowLostMouseFocusEvent"
instance IsEventData WindowGainedKeyboardFocusEventData where _eventData = _Ctor @"WindowGainedKeyboardFocusEvent"
instance IsEventData WindowLostKeyboardFocusEventData where _eventData = _Ctor @"WindowLostKeyboardFocusEvent"
instance IsEventData WindowClosedEventData where _eventData = _Ctor @"WindowClosedEvent"
instance IsEventData KeyboardEventData where _eventData = _Ctor @"KeyboardEvent"
instance IsEventData TextEditingEventData where _eventData = _Ctor @"TextEditingEvent"
instance IsEventData TextInputEventData where _eventData = _Ctor @"TextInputEvent"
instance IsEventData KeymapChangedEventData where _eventData = _Ctor @"KeymapChangedEvent" . from _Unit
instance IsEventData MouseMotionEventData where _eventData = _Ctor @"MouseMotionEvent"
instance IsEventData MouseButtonEventData where _eventData = _Ctor @"MouseButtonEvent"
instance IsEventData MouseWheelEventData where _eventData = _Ctor @"MouseWheelEvent"
instance IsEventData JoyAxisEventData where _eventData = _Ctor @"JoyAxisEvent"
instance IsEventData JoyBallEventData where _eventData = _Ctor @"JoyBallEvent"
instance IsEventData JoyHatEventData where _eventData = _Ctor @"JoyHatEvent"
instance IsEventData JoyButtonEventData where _eventData = _Ctor @"JoyButtonEvent"
instance IsEventData JoyDeviceEventData where _eventData = _Ctor @"JoyDeviceEvent"
instance IsEventData ControllerAxisEventData where _eventData = _Ctor @"ControllerAxisEvent"
instance IsEventData ControllerButtonEventData where _eventData = _Ctor @"ControllerButtonEvent"
instance IsEventData ControllerDeviceEventData where _eventData = _Ctor @"ControllerDeviceEvent"
instance IsEventData AudioDeviceEventData where _eventData = _Ctor @"AudioDeviceEvent"
instance IsEventData QuitEventData where _eventData = _Ctor @"QuitEvent" . from _Unit
instance IsEventData UserEventData where _eventData = _Ctor @"UserEvent"
instance IsEventData SysWMEventData where _eventData = _Ctor @"SysWMEvent"
instance IsEventData TouchFingerEventData where _eventData = _Ctor @"TouchFingerEvent"
instance IsEventData TouchFingerMotionEventData where _eventData = _Ctor @"TouchFingerMotionEvent"
instance IsEventData MultiGestureEventData where _eventData = _Ctor @"MultiGestureEvent"
instance IsEventData DollarGestureEventData where _eventData = _Ctor @"DollarGestureEvent"
instance IsEventData DropEventData where _eventData = _Ctor @"DropEvent"
instance IsEventData ClipboardUpdateEventData where _eventData = _Ctor @"ClipboardUpdateEvent" . from _Unit
instance IsEventData UnknownEventData where _eventData = _Ctor @"UnknownEvent"