module Ted.Editor.View where

import Ted.Editor.Common

-- | Describes a view on a buffer. Line and column specify the top left
-- character. The offsets allow fractional character position to be specified;
-- these values should remain in the range [0,1].
data View = View { viewLine :: Line
                 , viewColumn :: Column
                 , viewOffsetX :: Double
                 , viewOffsetY :: Double
                 }
 deriving (Eq, Show)
