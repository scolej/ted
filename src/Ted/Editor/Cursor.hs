module Ted.Editor.Cursor where

import Ted.Editor.Common

data Cursor
  = Cursor Line
           Column -- ^ Simple, normal cursor.
  | CursorSlice Line
                Column
                Int -- ^ Vertical slice cursor. Has a position and a height.
  | CursorLines Line
                Int -- ^ Selection of whole lines. Has a start line and a count of selected lines. Note that the count of lines might be negative to reflect lines above the cursor.
  | CursorRectangle Line
                    Column
                    Line
                    Column -- ^ Rectangular selection. Specifies the position of the top left and bottom right corners.
  | CursorRange Line
                Column
                Line
                Column -- ^ Normal range selection, everything in sequence, from position to position.
  deriving (Eq, Show)

cleanCursor :: Cursor -> Cursor
cleanCursor (Cursor line col) = Cursor line' col'
  where
    line' =
      if line < 1
        then 1
        else line
    col' =
      if col < 1
        then 1
        else col

moveCursor :: Direction -> Cursor -> Cursor
moveCursor d (Cursor line col) =
  cleanCursor $
  case d of
    DirUp -> Cursor (line - 1) col
    DirDown -> Cursor (line + 1) col
    DirLeft -> Cursor line (col - 1)
    DirRight -> Cursor line (col + 1)
-- | We need a concept of cursor position for all the different cursor types.
-- The cursor position returned here reflects the "active" end of the selection.
-- ie: if the selection is a rectangle, this function returns the position which
-- is manipulated by the user, not the "anchored" position.
-- cursorPos :: Cursor -> (Line, Column)
-- cursorPos (Cursor l c) = (l, c)
-- cursorPos (CursorSlice l c _) = (l, c)
-- cursorPos (CursorLines l _) = (l, 1)
