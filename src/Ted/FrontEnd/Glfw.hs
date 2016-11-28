module Ted.FrontEnd.Glfw where 

import Control.Concurrent.STM.TQueue
import Ted.Editor.Event

data GlfwFrontEnd = GlfwFrontEnd { eventQueue :: TQueue Event
                                 }
