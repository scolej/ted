module Ted.FrontEnd.Glfw
  ( startGlfwFrontEnd
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TQueue
import Control.Monad
import Debug.Trace
import Debug.Trace
import qualified Graphics.Rendering.FTGL as FTGL
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import System.Environment
import Ted.Editor
import Ted.Editor.Common
import Ted.Editor.Cursor
import qualified Ted.Editor.Event as Ev
import Ted.Editor.TextBuffer.String
import Ted.Editor.View
import Ted.Util

-- fontPath = "/usr/share/fonts/TTF/UbuntuMono-Bold.ttf" :: String
fontPath = "/usr/share/fonts/TTF/FSEX301-L2.ttf" :: String

fontPoints = 16 :: Int

startGlfwFrontEnd :: IO ()
startGlfwFrontEnd = do
  eventQueue <- atomically newTQueue
  args <- getArgs
  when (length args /= 1) (fail "Give me a file!")
  testLines <- fmap lines (readFile $ head args)
  True <- G.init
  Just win <- G.createWindow 640 480 "ted" Nothing Nothing
  G.makeContextCurrent (Just win)
  G.setErrorCallback (Just errorCallback)
  G.setKeyCallback win (Just $ keyCallback eventQueue)
  G.setCharCallback win (Just $ charCallback eventQueue)
  font <- FTGL.createTextureFont fontPath
  FTGL.setFontFaceSize font fontPoints 72
  fontWidth <- FTGL.getFontAdvance font "M"
  let loopf = mainLoop eventQueue font fontWidth win
      loop state =
        G.windowShouldClose win >>= \b -> unless b $ loopf state >>= loop
  loop $ initEditorState {bufferLines = StringListBuffer testLines}
  G.destroyWindow win
  G.terminate
  return ()

errorCallback :: G.ErrorCallback
errorCallback err = print

keyToEvent :: G.Key -> G.KeyState -> Maybe Ev.Event
keyToEvent key action
  | action `elem` [G.KeyState'Pressed, G.KeyState'Repeating] &&
      key == G.Key'Left = Just $ Ev.MotionBegins DirLeft
  | action `elem` [G.KeyState'Pressed, G.KeyState'Repeating] &&
      key == G.Key'Right = Just $ Ev.MotionBegins DirRight
  | action `elem` [G.KeyState'Pressed, G.KeyState'Repeating] && key == G.Key'Up =
    Just $ Ev.MotionBegins DirUp
  | action `elem` [G.KeyState'Pressed, G.KeyState'Repeating] &&
      key == G.Key'Down = Just $ Ev.MotionBegins DirDown
  | action == G.KeyState'Released && key == G.Key'Left =
    Just $ Ev.MotionEnds DirLeft
  | action == G.KeyState'Released && key == G.Key'Right =
    Just $ Ev.MotionEnds DirRight
  | action == G.KeyState'Released && key == G.Key'Up =
    Just $ Ev.MotionEnds DirUp
  | action == G.KeyState'Released && key == G.Key'Down =
    Just $ Ev.MotionEnds DirDown
  | otherwise =
    traceShow ("Not handled yet: " ++ show key ++ " " ++ show action) Nothing

keyCallback :: TQueue Ev.Event -> G.KeyCallback
keyCallback queue window key scancode action mods = do
  when
    (key == G.Key'Escape && action == G.KeyState'Pressed)
    (G.setWindowShouldClose window True)
  let me = keyToEvent key action
  case me of
    Nothing -> return ()
    Just e -> atomically (writeTQueue queue e)

charCallback :: TQueue Ev.Event -> G.CharCallback
charCallback queue window char =
  atomically $ writeTQueue queue (Ev.CharacterInput char)

drawingSetup :: (Int, Int) -> Float -> G.Window -> IO ()
drawingSetup (width, height) fontWidth win = do
  let ratio = fromIntegral width / fromIntegral height
      charsHeight = fi height / fi fontPoints
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  clearColor $= (Color4 1 1 1 1 :: Color4 GLfloat)
  clear [ColorBuffer]
  matrixMode $= Projection
  loadIdentity
  ortho 0 (ratio * charsHeight) charsHeight 0 1 (-1)
  matrixMode $= Modelview 0
  loadIdentity
  scale (fontWidth / fi fontPoints) 1 (1 :: GLfloat)
  translate $ v3 0 1 0
  -- scale 0.95 0.95 (1 :: GLfloat)

mainLoop
  :: TQueue Ev.Event
  -> FTGL.Font
  -> Float
  -> G.Window
  -> EditorState
  -> IO (EditorState)
mainLoop queue font fontWidth win es = do
  es' <- atomically $ processEvents queue es
  -- when (es /= es') (print es')
  (width, height) <- G.getFramebufferSize win
  drawingSetup (width, height) fontWidth win
  let vl = viewLine $ view es'
      vc = viewColumn $ view es'
  -- translate $ v3 (fi (-vc + 1)) (fi (-vl + 1)) 0
  -- Draw an origin.
  lineWidth $= 0.1
  color $ Color3 1.0 0.0 (0.0 :: GLfloat)
  renderPrimitive Lines $ do
    vertex $ Vertex3 0 0 (0 :: GLfloat)
    vertex $ Vertex3 0 1 (0 :: GLfloat)
    vertex $ Vertex3 0 0 (0 :: GLfloat)
    vertex $ Vertex3 1 0 (0 :: GLfloat)
  -- Draw cursor.
  lineWidth $= 2
  color $ Color3 1 0 (0 :: GLfloat)
  preservingMatrix $ do
    let Cursor l c = cursor es'
    translate $ v3 (fi $ c - vc) (fi $ l - vl) 0
    renderPrimitive LineLoop $ do
      vertex $ Vertex3 0 0 (0 :: GLfloat)
      vertex $ Vertex3 0 (-1) (0 :: GLfloat)
  -- Draw text.
  color $ Color3 0 0 (0 :: GLfloat)
  let StringListBuffer ls = bufferLines es'
      nlines = floor (fi height / fi fontPoints :: Float)
  simpleDrawLines nlines (fi fontPoints / fontWidth) font ls (view es')
  G.swapBuffers win
  G.pollEvents
  threadDelay 30000
  atomically $ writeTQueue queue (Ev.TimePasses 0.03)
  return es'

processEvents :: TQueue Ev.Event -> EditorState -> STM EditorState
processEvents queue state = do
  me <- tryReadTQueue queue
  case me of
    Nothing -> return state
    Just e ->
      let state' = processEvent e state
      in processEvents queue state'

v3 :: GLdouble -> GLdouble -> GLdouble -> Vector3 GLdouble
v3 = Vector3

simpleDrawLines :: Int -> Float -> FTGL.Font -> [String] -> View -> IO ()
simpleDrawLines nlines xrat font ls v =
  let s = 1 / fi fontPoints :: GLfloat
      drawLine l = do
        preservingMatrix $ do
          scale (s * xrat) (-s) 1
          FTGL.renderFont font (drop (fi $ (viewColumn v) - 1) l) FTGL.Front
        translate $ v3 0 1 0
  in do mapM_ drawLine $ (take nlines . drop (fi ((viewLine v) - 1))) ls
        return ()
