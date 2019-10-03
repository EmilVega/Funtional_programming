module Input where

import qualified Data.List as L
import Data.IORef
import Graphics.UI.GLUT

data InputData = InputData { onKeys :: [Key]              -- Keys that are currently down.
                           , mousePos :: Position         -- The current mouse position.
                           , previousMousePos :: Position -- The last recorded mouse position before the current mouse position. Useful for detecting the distance of mouse movements.
                           }

newInputData :: InputData
newInputData = InputData [] (Position 0 0) (Position 0 0) -- cambio [] []

mouseKeyboardCallback :: IORef InputData -> Key -> KeyState -> Modifiers -> Position -> IO ()
mouseKeyboardCallback inputsIORef key keyState _ newMousePos =
   if keyState == Down
      then inputsIORef $~ \x -> x { onKeys = if elem key (onKeys x)
                                                then onKeys x
                                                else key : (onKeys x)
                                   , mousePos = newMousePos }
      else inputsIORef $~ \x -> x { onKeys = L.delete key (onKeys x)
                                   , mousePos = newMousePos }

mouseMovementCallback :: IORef InputData -> Position -> IO ()
mouseMovementCallback inputsIORef newMousePos = inputsIORef $~ \x -> x { previousMousePos = (mousePos x)
                                                                        , mousePos = newMousePos }

cancelOnKeys :: InputData -> InputData
cancelOnKeys input = input { onKeys=[]}
