module Ted.Editor.TextBuffer where

import Ted.Editor.Common

class TextBuffer t where
  insertChar
  -- ^ Insert a new character on a line just before the Nth character.
  -- Inserting a character beyond the end of the line inserts spaces up until the insertion point.   
   :: Line -> Column -> Char -> t -> t
  deleteChar
  -- ^ Delete a character on a line.
  -- Deleting a character beyond the end of the line has no effect.
   :: Line -> Column -> t -> t
  deleteLine
  -- ^ Remove a line entirely.
   :: Line -> t -> t
  collapseLine
  -- ^ Join a line to the previous line.
   :: Line -> t -> t
  splitLine
  -- ^ Split a line into two lines.
   :: Line -> Column -> t -> t
