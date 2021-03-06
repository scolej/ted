module Ted.FrontEnd.Glfw
  ( startGlfwFrontEnd
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TQueue
import Control.Monad
import Data.Colour.RGBSpace
import Data.Time.Clock
import Data.Time.LocalTime
import Debug.Trace
import Debug.Trace
import qualified Graphics.Rendering.FTGL as FTGL
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import System.Environment
import Ted.Editor
import Ted.Editor.Common
import Ted.Editor.Cursor
import Ted.Editor.TextBuffer.String
import Ted.Editor.View
import qualified Ted.Fancy.Colours as FC
import Ted.Util

fontPath = "/usr/share/fonts/TTF/UbuntuMono-Bold.ttf" :: String

type Pixels = Int

cursorWidth = 2 :: Pixels

fontHeight = 16 :: Pixels

initWinHeight = 480 :: Pixels

initWinWidth = 640 :: Pixels

data UIState = UIState
  { uiEventQueue :: TQueue StateDelta
  , uiWindow :: G.Window
  , uiFont :: FTGL.Font
  , uiFontWidth :: Float
  , uiLastWrite :: UTCTime
  , uiLastEdit :: UTCTime
  , uiEditor :: EditorState
  }

startGlfwFrontEnd :: IO ()
startGlfwFrontEnd = do
  now <- getCurrentTime
  eventQueue <- atomically newTQueue
  args <- getArgs
  when (length args /= 1) (fail "Give me a file!")
  testLines <- fmap lines (readFile $ head args)
  True <- G.init
  Just win <- G.createWindow initWinWidth initWinHeight "ted" Nothing Nothing
  G.makeContextCurrent (Just win)
  G.setErrorCallback (Just errorCallback)
  G.setKeyCallback win (Just $ keyCallback eventQueue)
  G.setCharCallback win (Just $ charCallback eventQueue)
  font <- FTGL.createTextureFont fontPath
  FTGL.setFontFaceSize font fontHeight 72
  fontWidth <- FTGL.getFontAdvance font "M"
  G.setWindowSizeCallback win (Just $ sizeCallback eventQueue fontWidth)
  let loop state =
        G.windowShouldClose win >>= \b -> unless b $ (mainLoop state) >>= loop
  loop $
    UIState
    { uiEventQueue = eventQueue
    , uiFont = font
    , uiFontWidth = fontWidth
    , uiWindow = win
    , uiLastEdit = now
    , uiLastWrite = now
    , uiEditor =
        initEditorState
        { bufferLines = StringListBuffer testLines
        , view =
            View
              1
              1
              (floor $ fi initWinHeight / fi fontHeight)
              (floor $ fi initWinWidth / fontWidth)
        }
    }
  G.destroyWindow win
  G.terminate
  return ()

errorCallback :: G.ErrorCallback
errorCallback err = print

-- | Map key inputs to editor state transformations.
keyToDelta :: G.Key -> G.KeyState -> G.ModifierKeys -> StateDelta
keyToDelta key a mods
  | a `elem` [G.KeyState'Pressed, G.KeyState'Repeating] && key == G.Key'Left =
    motionBegins DirLeft
  | a `elem` [G.KeyState'Pressed, G.KeyState'Repeating] && key == G.Key'Right =
    motionBegins DirRight
  | a `elem` [G.KeyState'Pressed, G.KeyState'Repeating] && key == G.Key'Up =
    motionBegins DirUp
  | a `elem` [G.KeyState'Pressed, G.KeyState'Repeating] && key == G.Key'Down =
    motionBegins DirDown
  | a `elem` [G.KeyState'Pressed, G.KeyState'Repeating] &&
      key == G.Key'Backspace = backspace
  | a == G.KeyState'Released && key == G.Key'Left = motionEnds DirLeft
  | a == G.KeyState'Released && key == G.Key'Right = motionEnds DirRight
  | a == G.KeyState'Released && key == G.Key'Up = motionEnds DirUp
  | a == G.KeyState'Released && key == G.Key'Down = motionEnds DirDown
  | a == G.KeyState'Pressed && key == G.Key'D && G.modifierKeysControl mods =
    deleteWholeLine
  | a == G.KeyState'Pressed && key == G.Key'End = gotoEndOfLine
  | a == G.KeyState'Pressed && key == G.Key'Home = gotoStartOfLine
  | a == G.KeyState'Pressed && key == G.Key'Enter = splitLineAtCursor
  | otherwise = traceShow ("Not handled yet: " ++ show key ++ " " ++ show a) id

keyCallback :: TQueue StateDelta -> G.KeyCallback
keyCallback queue window key scancode action mods = do
  when
    (key == G.Key'Escape && action == G.KeyState'Pressed)
    (G.setWindowShouldClose window True)
  atomically (writeTQueue queue (keyToDelta key action mods))

charCallback :: TQueue StateDelta -> G.CharCallback
charCallback queue window char =
  atomically $ writeTQueue queue (characterInput char)

sizeCallback :: TQueue StateDelta -> Float -> G.WindowSizeCallback
sizeCallback queue fontWidth window width height =
  let lines = floor $ fi height / fi fontHeight
      cols = floor $ fi width / fontWidth
  in atomically $
     writeTQueue queue (ensureVisibleCursor . resizeView lines cols)

drawingSetup :: (Int, Int) -> Float -> G.Window -> RGB GLfloat -> IO ()
drawingSetup (width, height) fontWidth win colourBg = do
  let ratio = fromIntegral width / fromIntegral height
      charsHeight = fi height / fi fontHeight
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  clearColor $=
    (Color4
       (channelRed colourBg)
       (channelGreen colourBg)
       (channelBlue colourBg)
       1 :: Color4 GLfloat)
  clear [ColorBuffer]
  matrixMode $= Projection
  loadIdentity
  ortho 0 (ratio * charsHeight) charsHeight 0 1 (-1)
  matrixMode $= Modelview 0
  loadIdentity
  scale (fontWidth / fi fontHeight) 1 (1 :: GLfloat)
  translate $ v3 1 1 0

mainLoop :: UIState -> IO (UIState)
mainLoop ui = do
  tod <-
    (fromRational .
     timeOfDayToDayFraction . localTimeOfDay . zonedTimeToLocalTime) <$>
    getZonedTime
  let colourFg = FC.slidingFg tod
  let colourBg = FC.slidingBg tod
  es' <- atomically $ processDeltas (uiEventQueue ui) (uiEditor ui)
  (width, height) <- G.getFramebufferSize (uiWindow ui)
  drawingSetup (width, height) (uiFontWidth ui) (uiWindow ui) colourBg
  let vl = viewLine $ view es'
      vc = viewColumn $ view es'
  -- Draw cursor.
  lineWidth $= fi cursorWidth
  color $ Color3 1 0 (0 :: GLfloat)
  preservingMatrix $ do
    let Cursor l c = cursor es'
    translate $ v3 (fi $ c - vc) (fi $ l - vl) 0
    renderPrimitive LineLoop $ do
      vertex $ Vertex3 0 0 (0 :: GLfloat)
      vertex $ Vertex3 0 (-1) (0 :: GLfloat)
  -- Draw text.
  color $
    Color3 (channelRed colourFg) (channelGreen colourFg) (channelBlue colourFg)
  let StringListBuffer ls = bufferLines es'
      nlines = floor (fi height / fi fontHeight :: Float)
  simpleDrawLines
    nlines
    (fi fontHeight / (uiFontWidth ui))
    (uiFont ui)
    ls
    (view es')
  G.swapBuffers (uiWindow ui)
  G.pollEvents
  threadDelay 30000
  atomically $ writeTQueue (uiEventQueue ui) (timePasses 0.03)
  return $ ui {uiEditor = es'}

-- | Read state changes off the queue, applying them in turn, until there are none left.
processDeltas :: TQueue StateDelta -> EditorState -> STM EditorState
processDeltas queue state = do
  me <- tryReadTQueue queue
  case me of
    Nothing -> return state
    Just e ->
      let state' = e state
      in processDeltas queue state'

v3 :: GLdouble -> GLdouble -> GLdouble -> Vector3 GLdouble
v3 = Vector3

simpleDrawLines :: Int -> Float -> FTGL.Font -> [String] -> View -> IO ()
simpleDrawLines nlines xrat font ls v =
  let s = 1 / fi fontHeight :: GLfloat
      drawLine l = do
        preservingMatrix $ do
          scale (s * xrat) (-s) 1
          FTGL.renderFont font (drop (fi $ (viewColumn v) - 1) l) FTGL.Front
        translate $ v3 0 1 0
  in do mapM_ drawLine $ (take nlines . drop (fi ((viewLine v) - 1))) ls
        return ()
