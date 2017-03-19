module Ted.Editor.Cursor where

import Ted.Editor.Common

-- | Simple, normal cursor.
data PointCursor =
  PointCursor LineCol

-- | Normal range selection, everything in sequence, from position to position.
data RangeCursor =
  RangeCursor LineCol
              LineCol

-- | Vertical slice cursor.
data SliceCursor =
  SliceCursor LineCol
              Line

-- | Selection of whole lines.
-- The first line is the active end.
data LinesCursor =
  LinesCursor Line
              Line

-- | Rectangular selection.
-- Specifies a rectangle with two corners.
-- The first position is the active end.
data RectangleCursor =
  RectangleCursor LineCol
                  LineCol

-- | The sum of all cursors.
data AnyCursor
  = ACPoint PointCursor
  | ACRange RangeCursor
  | ACSlice SliceCursor
  | ACLines LinesCursor
  | ACRectangle RectangleCursor

-- | Move a cursor in a direction.
nudgeCursor :: Direction -> AnyCursor -> AnyCursor
nudgeCursor = undefined

-- | Change the shape of the cursor by moving the active end in a direction.
warpCursor :: Direction -> AnyCursor -> AnyCursor
warpCursor = undefined

-- | Switch the active and inactive ends of the cursor (ie: exchange point and mark).
toggleCursorHandle :: AnyCursor -> AnyCursor
toggleCursorHandle = undefined

-- | Take any cursor and reduce it back to a point.
collapseCursor :: AnyCursor -> PointCursor
collapseCursor = undefined
