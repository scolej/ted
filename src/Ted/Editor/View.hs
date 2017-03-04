module Ted.Editor.View where

import Ted.Editor.Common

-- | Describes a view on a buffer. Line and column specify the top left
-- character. Lines and columns specify how many lines and columns are visible.
data View = View
  { viewLine :: Line
  , viewColumn :: Column
  , viewLines :: Int
  , viewColumns :: Int
  } deriving (Eq, Show)
