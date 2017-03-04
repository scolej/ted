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
  -- , interactionState :: InteractionState
  } deriving (Eq, Show)

initEditorState :: EditorState
initEditorState =
  EditorState
  { filePath = Nothing
  , bufferLines = (StringListBuffer [])
  , cursor = (Cursor 1 1)
  , view = (View 1 1 10 80)
    -- initInteractionState
  }

motionBegins :: Direction -> StateDelta
motionBegins d = updateCursor d

motionEnds :: Direction -> StateDelta
motionEnds d = id

characterInput :: Char -> StateDelta
characterInput c = insertCharacter c

insertCharacter :: Char -> EditorState -> EditorState
insertCharacter char es = updateCursor DirRight $ es {bufferLines = newLines}
  where
    Cursor line col = cursor es
    oldLines = bufferLines es
    newLines = insertChar line col char oldLines

-- updateInteractionState :: (InteractionState -> InteractionState)
--                        -> EditorState
--                        -> EditorState
-- updateInteractionState f es0 =
--   let is0 = interactionState es0
--   in es0 {interactionState = f is0}
timePasses :: Float -> StateDelta
timePasses t = id

updateCursor :: Direction -> EditorState -> EditorState
updateCursor d es =
  let c0 = cursor es
      es' = es {cursor = moveCursor d c0}
  in trace (show (cursor es') ++ show (view es')) (ensureVisibleCursor es')

-- | Move the view to make sure the cursor can be seen.
ensureVisibleCursor :: EditorState -> EditorState
ensureVisibleCursor es = es {view = View l' c' h w}
  where
    v = view es
    Cursor cl cc = cursor es
    l = viewLine v
    c = viewColumn v
    h = viewLines v
    w = viewColumns v
    l'
      | cl < l = cl
      | cl > l + h = l + (cl - (l + h))
      | otherwise = l
    c'
      | cc < c = cc
      | cc > c + w = c + (cc - (c + w))
      | otherwise = c
