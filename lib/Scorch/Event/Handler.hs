module Scorch.Event.Handler where

import Scorch.Event.Type

data EventResult = EventCanceled | EventDidSomething

type EventHandler m = EventContext -> Event -> m (Maybe EventResult)
