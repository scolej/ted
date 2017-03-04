module Ted.Editor.Interaction where

import Ted.Editor.Common

data InputMotionX
  = IMXLeft
  | IMXNone
  | IMXRight
  deriving (Eq, Show)

data InputMotionY
  = IMYUp
  | IMYNone
  | IMYDown
  deriving (Eq, Show)

data InteractionState = InteractionState
  { inputMotionX :: InputMotionX
  , inputMotionY :: InputMotionY
  } deriving (Eq, Show)

initInteractionState = InteractionState IMXNone IMYNone

applyMotion :: Direction -> InteractionState -> InteractionState
applyMotion d ints =
  case d of
    DirLeft -> ints {inputMotionX = IMXLeft}
    DirRight -> ints {inputMotionX = IMXRight}
    DirUp -> ints {inputMotionY = IMYUp}
    DirDown -> ints {inputMotionY = IMYDown}

removeMotion :: Direction -> InteractionState -> InteractionState
removeMotion d ints =
  case d of
    DirLeft -> ints {inputMotionX = IMXNone}
    DirRight -> ints {inputMotionX = IMXNone}
    DirUp -> ints {inputMotionY = IMYNone}
    DirDown -> ints {inputMotionY = IMYNone}
