module Ted.Editor.Common where

data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Eq, Show)

type Line = Int

type Column = Int
