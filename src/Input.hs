module Input
       ( inputHandler
       ) where

import qualified SDL
import Control.Monad      (void)
import Control.Concurrent

inputHandler :: MVar Bool -> MVar Bool -> IO ()
inputHandler mouseHeld quitPressed = do
  event <- SDL.waitEvent
  let eventMouseButtonStatus event mouseButton inputMotion =
        case SDL.eventPayload event of
          SDL.MouseButtonEvent mouseEvent ->
            SDL.mouseButtonEventButton mouseEvent == mouseButton &&
            SDL.mouseButtonEventMotion mouseEvent == inputMotion
          _ -> False
  let leftMouseButtonPressed = eventMouseButtonStatus event SDL.ButtonLeft SDL.Pressed
      leftMouseButtonReleased = eventMouseButtonStatus event SDL.ButtonLeft SDL.Released
      quitButtonPressed = case SDL.eventPayload event of
                            SDL.QuitEvent -> True
                            _             -> False

  if quitButtonPressed
     then void $ swapMVar quitPressed True
     else return ()
  if leftMouseButtonPressed
     then void $ swapMVar mouseHeld True
     else return ()
  if leftMouseButtonReleased
     then void $ swapMVar mouseHeld False
     else return ()

  inputHandler mouseHeld quitPressed
