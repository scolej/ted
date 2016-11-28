module Ted.Editor where

import Ted.Editor.Cursor
import Ted.Editor.Common
import Ted.Editor.View

data EditorState =
     EditorState { filePath :: Maybe String
                 , bufferLines :: [String]
                 , cursor :: Cursor
                 , cursorMotion :: CursorMotion
                 , view :: View
                 }

applyMotion :: CursorMotion -> Direction -> CursorMotion
applyMotion (CursorMotion x y) d
  | d == DirLeft  = CursorMotion (Just DirXLeft) y
  | d == DirRight = CursorMotion (Just DirXRight) y
  | d == DirUp    = CursorMotion x (Just DirYUp)
  | d == DirDown  = CursorMotion x (Just DirYDown)

removeMotion :: CursorMotion -> Direction -> CursorMotion
removeMotion (CursorMotion x y) d
  | d == DirLeft  = CursorMotion Nothing y
  | d == DirRight = CursorMotion Nothing y
  | d == DirUp    = CursorMotion x Nothing
  | d == DirDown  = CursorMotion x Nothing

editorEvolve :: EditorState -> Event -> EditorState
editorEvolve es0 (EventTimePasses       t) = editorTimePasses es0 t
editorEvolve es0 (EventCommenceMoving dir) = let cm0 = cursorMotion e0 in es0 { cursorMotion = applyMotion  cm0 dir }
editorEvolve es0 (EventConcludeMoving dir) = let cm0 = cursorMotion e0 in es0 { cursorMotion = removeMotion cm0 dir }

editorTimePasses :: EditorState -> Float -> EditorState
editorTimePasses es0 t = undefined
