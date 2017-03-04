module Ted.Editor.Event where

import Ted.Editor.Common

data Event
  = TimePasses Float
  | MotionBegins Direction
  | MotionEnds Direction
  | CharacterInput Char
  deriving (Show)
