module Ted.Editor
  ( processEvent
  , EditorState(..)
  , initEditorState
  ) where

import Debug.Trace
import Ted.Editor.Common
import Ted.Editor.Cursor
import Ted.Editor.Event
import Ted.Editor.Interaction
import Ted.Editor.TextBuffer
import Ted.Editor.TextBuffer.String
import Ted.Editor.View

data EditorState = EditorState
  { filePath :: Maybe String
  , bufferLines :: StringListBuffer
  , cursor :: Cursor
  , view :: View
  , interactionState :: InteractionState
  } deriving (Eq, Show)

initEditorState :: EditorState
initEditorState =
  EditorState
    Nothing
    (StringListBuffer [])
    (Cursor 1 1)
    (View 1 1 10 80)
    initInteractionState

processEvent :: Event -> EditorState -> EditorState
processEvent evt =
  case evt of
    TimePasses t -> editorTimePasses t
              -- MotionBegins dir -> id -- updateInteractionState (applyMotion dir)
    MotionEnds dir -> id -- updateInteractionState (removeMotion dir)
    MotionBegins dir -> updateCursor dir
    CharacterInput c -> insertCharacter c
    e -> trace ("Event not handled yet: " ++ show e) id

insertCharacter :: Char -> EditorState -> EditorState
insertCharacter char es = updateCursor DirRight $ es {bufferLines = newLines}
  where
    Cursor line col = cursor es
    oldLines = bufferLines es
    newLines = insertChar line col char oldLines

updateInteractionState :: (InteractionState -> InteractionState)
                       -> EditorState
                       -> EditorState
updateInteractionState f es0 =
  let is0 = interactionState es0
  in es0 {interactionState = f is0}

editorTimePasses :: Float -> EditorState -> EditorState
editorTimePasses t es0 = es0

updateCursor :: Direction -> EditorState -> EditorState
updateCursor d es =
  let c0 = cursor es
      es' = es {cursor = moveCursor d c0}
  in trace (show (cursor es') ++ show (view es')) (ensureVisibleCursor es')

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
