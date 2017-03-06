module Ted.Editor.TextBuffer.String where

import Ted.Editor.TextBuffer

newtype StringListBuffer =
  StringListBuffer [String]
  deriving (Eq, Show)

instance TextBuffer StringListBuffer where
  insertChar line col char (StringListBuffer oldLines) =
    StringListBuffer $ skipsStart ++ [modified] ++ skipsEnd
    where
      skipsStart = take (line - 1) oldLines
      skipsEnd = drop (line) oldLines
      l = oldLines !! (line - 1)
      l' = take (max col (length l)) (l ++ repeat ' ')
      modified = skipsCharStart ++ [char] ++ skipsCharEnd
      skipsCharStart = take (col - 1) l'
      skipsCharEnd = drop (col - 1) l'
  deleteChar line col (StringListBuffer oldLines) =
    StringListBuffer $ skipsStart ++ [modified] ++ skipsEnd
    where
      skipsStart = take (line - 1) oldLines
      skipsEnd = drop (line) oldLines
      l = oldLines !! (line - 1)
      modified = take (col - 2) l ++ drop (col - 1) l
