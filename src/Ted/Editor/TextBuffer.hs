module Ted.Editor.TextBuffer where

import Ted.Editor.Common

class TextBuffer t where
  -- | Insert a new character on a line just before the Nth character.
  -- Inserting a character beyond the end of the line inserts spaces up until the insertion point.
  insertChar :: Line -> Column -> Char -> t -> t
  -- | Delete a character on a line.
  -- Deleting a character beyond the end of the line has no effect.
  deleteChar :: Line -> Column -> t -> t
  -- | Remove a line entirely.
  deleteLine :: Line -> t -> t
  -- | Join a line to the previous line.
  collapseLine :: Line -> t -> t
  -- | Split a line into two lines.
  splitLine :: Line -> Column -> t -> t
