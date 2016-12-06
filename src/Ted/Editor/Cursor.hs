module Ted.Editor.Cursor where

import Ted.Editor.Common

data Cursor = Cursor Line Column -- ^ Simple, normal cursor.
            | CursorSlice Line Column Int -- ^ Vertical slice cursor. Has a position and a height.
            | CursorLines Line Int -- ^ Selection of whole lines. Has a start line and a count of selected lines.
            | CursorRectangle Line Column Line Column -- ^ Rectangular selection. Specifies the position of the top left and bottom right corners.
            | CursorRange Line Column Line Column -- ^ Normal range selection, everything in sequence, from position to position.
 deriving (Eq, Show)

cleanCursor :: Cursor -> Cursor
cleanCursor (Cursor line col) = Cursor line' col'
  where line' = if line < 1 then 1 else line
        col' = if col < 1 then 1 else col

moveCursor :: Direction -> Cursor -> Cursor
moveCursor d (Cursor line col) = 
  cleanCursor $ case d of DirUp   -> Cursor (line - 1) col
                          DirDown -> Cursor (line + 1) col
                          DirLeft -> Cursor line (col - 1)
                          DirRight-> Cursor line (col + 1)
