module Ted.Editor.TextBuffer.String where

import Ted.Editor.TextBuffer

newtype StringListBuffer =
  StringListBuffer [String]
  deriving (Eq, Show)

perforate :: Int -> [a] -> Maybe ([a], a, [a])
perforate n xs
  | n < 0 || n >= length xs = Nothing
  | otherwise = Just (take n xs, xs !! n, drop (n + 1) xs)

instance TextBuffer StringListBuffer where
  insertChar line col char old@(StringListBuffer oldLines) =
    case perforate (line - 1) oldLines of
      Nothing -> old
      Just (skipsStart, l, skipsEnd) ->
        StringListBuffer $ skipsStart ++ [modified] ++ skipsEnd
        where l' = take (max col (length l)) (l ++ repeat ' ')
              modified = skipsCharStart ++ [char] ++ skipsCharEnd
              skipsCharStart = take (col - 1) l'
              skipsCharEnd = drop (col - 1) l'
  deleteChar line col old@(StringListBuffer oldLines) =
    case newLines of Nothing -> old
                     Just ls -> StringListBuffer ls
    where newLines = do
            (lsA, l, lsB) <- perforate (line - 1) oldLines
            (csA, _, csB) <- perforate (col - 1) l
            return $ lsA ++ [csA ++ csB] ++ lsB
  deleteLine line old@(StringListBuffer oldLines) =
    case newLines of Nothing -> old
                     Just ls -> StringListBuffer ls
    where newLines = do
             (lsA, l, lsB) <- perforate (line - 1) oldLines
             return $ lsA ++ lsB
  collapseLine line old@(StringListBuffer oldLines) =
    case newLines of Nothing -> old
                     Just ls -> StringListBuffer ls
    where newLines = do
            (lsA, a, _) <- perforate (line - 2) oldLines
            (_, b, lsB) <- perforate (line - 1) oldLines
            return $ lsA ++ [a ++ b] ++ lsB
