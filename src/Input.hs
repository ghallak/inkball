module Input where

import qualified SDL
import Control.Monad      (void)
import Control.Concurrent

inputHandler :: MVar Bool -> IO ()
inputHandler mouseHeld = do
  event <- SDL.waitEvent
  let eventMouseButtonStatus event mouseButton inputMotion =
        case SDL.eventPayload event of
          SDL.MouseButtonEvent mouseEvent ->
            SDL.mouseButtonEventButton mouseEvent == mouseButton &&
            SDL.mouseButtonEventMotion mouseEvent == inputMotion
          _ -> False
  let leftMouseButtonPressed = eventMouseButtonStatus event SDL.ButtonLeft SDL.Pressed
      leftMouseButtonReleased = eventMouseButtonStatus event SDL.ButtonLeft SDL.Released

  if leftMouseButtonPressed
     then void $ swapMVar mouseHeld True
     else return ()
  if leftMouseButtonReleased
     then void $ swapMVar mouseHeld False
     else return ()

  inputHandler mouseHeld
