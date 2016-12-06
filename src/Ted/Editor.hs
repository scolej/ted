module Ted.Editor 
( processEvent
, EditorState (..)
, initEditorState
) where

import Ted.Editor.Common
import Ted.Editor.Cursor
import Ted.Editor.Event
import Ted.Editor.View
import Ted.Editor.Interaction
import Ted.Editor.TextBuffer
import Ted.Editor.TextBuffer.String
import Debug.Trace

data EditorState =
     EditorState { filePath :: Maybe String
                 , bufferLines :: StringListBuffer
                 , cursor :: Cursor
                 , view :: View
                 , interactionState :: InteractionState
                 }
  deriving (Eq, Show)

initEditorState :: EditorState
initEditorState = EditorState Nothing 
                              (StringListBuffer [])
                              (Cursor 1 1)
                              (View 1 1 0 0)
                              initInteractionState

processEvent :: Event -> EditorState -> EditorState
processEvent evt = 
  case evt of TimePasses t     -> editorTimePasses t
              -- MotionBegins dir -> id -- updateInteractionState (applyMotion dir)
              MotionEnds dir   -> id -- updateInteractionState (removeMotion dir)
              MotionBegins dir -> updateCursor dir
              CharacterInput c -> insertCharacter c
              e                -> trace ("Event not handled yet: " ++ show e) id

insertCharacter :: Char -> EditorState -> EditorState
insertCharacter char es = updateCursor DirRight $ es { bufferLines = newLines }
  where Cursor line col = cursor es
        oldLines = bufferLines es
        newLines = insertChar line col char oldLines

updateInteractionState :: (InteractionState -> InteractionState) -> EditorState -> EditorState
updateInteractionState f es0 =
  let is0 = interactionState es0
  in es0 { interactionState = f is0 }

editorTimePasses :: Float -> EditorState -> EditorState
editorTimePasses t es0 = es0

updateCursor :: Direction -> EditorState -> EditorState
updateCursor d es = 
  let c0 = cursor es
      es' = es { cursor = moveCursor d c0 }
  in trace (show (cursor es') ++ show (view es')) (ensureVisibleCursor es')

ensureVisibleCursor :: EditorState -> EditorState
ensureVisibleCursor es = es { view = View line' col' 0 0 }
  where Cursor line col = cursor es
        View vLine vCol _ _ = view es
        line' = if abs (line - vLine) > 10 then (line - 10) * signum (line - vLine) else vLine
        col' = if abs (col - vCol) > 10 then col else vCol
