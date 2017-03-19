module Ted.Editor.Common
  ( Direction(..)
  , Line
  , line
  , Column
  , column
  , LineCol
  ) where

-- | A direction on a 2D plane.
data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Eq, Show)

-- | A line in a file.
data Line =
  Line Integer
  deriving (Eq, Show)

-- | Get a line by index. Nothing is returned for values less than 1.
line :: Integer -> Maybe Line
line i
  | i <= 0 = Nothing
  | otherwise = Just $ Line i

-- | A column in a file.
data Column =
  Column Integer
  deriving (Eq, Show)

-- | Get a column by index. Nothing is returned for values less than 1.
column :: Integer -> Maybe Column
column i
  | i <= 0 = Nothing
  | otherwise = Just $ Column i

-- | A position in a file by line and column.
data LineCol =
  LineCol Line
          Column
  deriving (Eq, Show)

instance Ord LineCol where
  (LineCol (Line al) (Column ac)) `compare` (LineCol (Line bl) (Column bc))
    | al > bl = GT
    | al < bl = LT
    | al == bl && ac > bc = GT
    | al == bl && ac < bc = LT
    | al == bl && ac == bc = EQ
    | otherwise = error "Bad bad bad."
