module Ted.Editor where

import Debug.Trace
import Ted.Editor.Common
import Ted.Editor.Cursor
import Ted.Editor.Interaction
import Ted.Editor.TextBuffer
import Ted.Editor.TextBuffer.String
import Ted.Editor.View

-- | Functions which transform the editor state.
type StateDelta = EditorState -> EditorState

data EditorState = EditorState
  { filePath :: Maybe String
  , bufferLines :: StringListBuffer
  , cursor :: Cursor
  , view :: View
  } deriving (Eq, Show)

initEditorState :: EditorState
initEditorState =
  EditorState
  { filePath = Nothing
  , bufferLines = (StringListBuffer [])
  , cursor = (Cursor 1 1)
  , view = (View 1 1 10 80)
  }

motionBegins :: Direction -> StateDelta
motionBegins d = updateCursor d

motionEnds :: Direction -> StateDelta
motionEnds d = id

backspace :: StateDelta
backspace es =
  let Cursor l c = cursor es
      buffer = bufferLines es
  in if c == 1
     then let buffer' = collapseLine l buffer
          in (gotoEndOfLine $ updateCursor DirUp es) {bufferLines = buffer'}
     else let buffer' = deleteChar l (c - 1) buffer
          in updateCursor DirLeft $ es {bufferLines = buffer'}

deleteWholeLine :: StateDelta
deleteWholeLine es = es { bufferLines = buffer' }
  where buffer' = deleteLine l (bufferLines es)
        Cursor l _ = cursor es

characterInput :: Char -> StateDelta
characterInput c = insertCharacter c

insertCharacter :: Char -> StateDelta
insertCharacter char es = updateCursor DirRight $ es {bufferLines = newLines}
  where
    Cursor line col = cursor es
    oldLines = bufferLines es
    newLines = insertChar line col char oldLines

splitLineAtCursor :: StateDelta
splitLineAtCursor es = gotoStartOfLine $ updateCursor DirDown $ es {bufferLines = newLines}
  where newLines = splitLine l c (bufferLines es)
        Cursor l c = cursor es

timePasses :: Float -> StateDelta
timePasses t = id

gotoEndOfLine :: StateDelta
gotoEndOfLine es = es { cursor = Cursor l c' }
  where Cursor l c = cursor es
        StringListBuffer lines = bufferLines es
        c' = 1 + (length $ lines !! (l - 1)) -- FIXME

gotoStartOfLine :: StateDelta
gotoStartOfLine es = es { cursor = Cursor l 1 }
  where Cursor l c = cursor es

updateCursor :: Direction -> StateDelta
updateCursor d es =
  let c0 = cursor es
      es' = es {cursor = moveCursor d c0}
  in ensureVisibleCursor es'

-- | Move the view to make sure the cursor can be seen.
ensureVisibleCursor :: StateDelta
ensureVisibleCursor es = es {view = View l' c' h w}
  where
    v = view es
    Cursor cl cc = cursor es
    l = viewLine v
    c = viewColumn v
    h = viewLines v
    w = viewColumns v
    b = l + h -- Bottom.
    r = c + w -- Right.
    l'
      | cl < l = cl
      | cl > b = l + (cl - b)
      | otherwise = l
    c'
      | cc < c = cc
      | cc > r = c + (cc - r)
      | otherwise = c

resizeView :: Int -> Int -> StateDelta
resizeView lines columns es =
  es {view = (view es) {viewLines = lines, viewColumns = columns}}
