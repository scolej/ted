module Ted.Editor.TextBuffer where

import Ted.Editor.Common

class TextBuffer t
  -- getLine :: Line -> t -> String
  -- insertLine :: Line -> String -> t -> t
  -- deleteLine :: Line -> t -> t
                                  where
  insertChar :: Line -> Column -> Char -> t -> t
  deleteChar :: Line -> Column -> t -> t
