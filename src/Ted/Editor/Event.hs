module Ted.Editor.Event where

import Ted.Editor.Common

data Event = EventTimePasses Float
           | EventCharInput Char
           | EventCommenceMoving Direction
           | EventConcludeMoving Direction
