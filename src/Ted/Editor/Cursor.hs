module Ted.Editor.Cursor where

import Ted.Editor.Common

data Cursor = Cursor Line Column -- ^ Simple, normal cursor.
            | CursorSlice Line Column Int -- ^ Vertical slice cursor. Has a position and a height.
            | CursorLines Line Int -- ^ Selection of whole lines. Has a start line and a count of selected lines.
            | CursorRectangle Line Column Line Column -- ^ Rectangular selection. Specifies the position of the top left and bottom right corners.
            | CursorRange Line Column Line Column -- ^ Normal range selection, everything in sequence, from position to position.

data CursorMotion = CursorMotion (Maybe DirectionX) (Maybe DirectionY)
