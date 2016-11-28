module Ted.Editor.Common where

type Line = Int

type Column = Int

data Direction = DirUp | DirDown | DirLeft | DirRight

data DirectionX = DirXRight | DirXLeft

data DirectionY = DirYUp | DirYDown
