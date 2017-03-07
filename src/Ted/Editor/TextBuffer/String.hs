module Ted.Editor.TextBuffer.String where

import Ted.Editor.TextBuffer

(<&>) = flip fmap

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
  deleteChar line 1 old@(StringListBuffer oldLines) =
    case (perforate (line - 2) oldLines) >>=
         (\(as, a, _) ->
            (perforate (line - 1) oldLines) <&> \(_, b, bs) -> (as, a, b, bs)) of
      Nothing -> old
      Just (as, a, b, bs) -> StringListBuffer $ as ++ [a ++ b] ++ bs
  deleteChar line col old@(StringListBuffer oldLines) =
    case perforate (line - 1) oldLines of
      Nothing -> old
      Just (skipsStart, l, skipsEnd) ->
        StringListBuffer $ skipsStart ++ [modified] ++ skipsEnd
        where modified = take (col - 2) l ++ drop (col - 1) l
