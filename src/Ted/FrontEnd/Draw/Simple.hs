module Ted.Draw.Simple where

import Ted.Editor.View
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.Rendering.OpenGL as GL

v3 :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GL.Vector3 GL.GLdouble
v3 = GL.Vector3

simpleDrawLines :: FTGL.Font -> [String] -> View -> IO ()
simpleDrawLines font ls (View line col ox oy) =
  let drawLine l = do
        FTGL.renderFont font (drop (fromIntegral col) l) FTGL.Front
        GL.translate $ v3 0 (-14) 0
  in do GL.translate $ v3 (-ox) (-oy) 0
        mapM_ drawLine (drop (fromIntegral (line - 1)) ls)
        return ()
     
  
