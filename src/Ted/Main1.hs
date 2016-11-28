import Control.Monad
import Debug.Trace
import Graphics.Rendering.OpenGL
import Ted.Editor.View
import Ted.FrontEnd.Draw.Simple
import Ted.Util
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.UI.GLFW as G

fontPath = "/usr/share/fonts/TTF/UbuntuMono-Regular.ttf" :: String
fontPoints = 12 :: Int

main :: IO ()
main = do
  testLines <- fmap lines (readFile "Ted/Main1.hs")
  G.setErrorCallback (Just errorCallback)
  True <- G.init
  Just w <- G.createWindow 640 480 "ted" Nothing Nothing
  G.makeContextCurrent (Just w)
  G.setKeyCallback w (Just keyCallback)
  font <- FTGL.createTextureFont fontPath
  FTGL.setFontFaceSize font fontPoints 72
  mainLoop testLines font w
  G.destroyWindow w
  G.terminate
  return ()

errorCallback :: G.ErrorCallback
errorCallback err = print 

keyCallback :: G.KeyCallback
keyCallback window key scancode action mods =
  when (key == G.Key'Escape && action == G.KeyState'Pressed) $
    G.setWindowShouldClose window True


mainLoop :: [String] -> FTGL.Font -> G.Window -> IO ()
mainLoop ls font win = G.windowShouldClose win >>= \b -> unless b $ do

  (width, height) <- G.getFramebufferSize win
  let ratio = fromIntegral width / fromIntegral height
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))

  clearColor $= (Color4 1 1 1 1 :: Color4 GLfloat)
  clear [ColorBuffer]
  
  matrixMode $= Projection
  loadIdentity
  ortho 0 (fi width) 0 (fi width / ratio) 1 (-1)
  
  matrixMode $= Modelview 0
  loadIdentity
  translate $ v3 0 (fi $ height - fontPoints) 0
  
  color (Color3 0 0 0 :: Color3 GLdouble)

  simpleDrawLines font ls (View 10 10 5 0)

  G.swapBuffers win
  G.pollEvents

  mainLoop ls font win
