module Lib where

import qualified SDL
import qualified Foreign.C.Types as CTypes

import GameObjects
import Physics
import Geometry
import Graphics

import Data.Bool              (bool)
import Data.Maybe             (isJust, fromJust)
import Control.Monad          (unless)
import Data.Text              (pack)
import Data.List              (find)
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types        (CInt)
import SDL                    (($=))
import System.IO              (readFile)
import Control.Concurrent     (threadDelay)

nextInkState :: InkLine -> [Ball] -> Maybe InkLine
nextInkState inkLine balls =
  if any (\ball -> collide ball inkLine) balls
     then Nothing
     else Just inkLine

nextState :: Ball -> [Ball] -> [Block] -> [Sink] -> [InkLine] -> Maybe Ball
nextState ball balls blocks sinks inkLines =
  case find (collide ball) sinks of
    Just _  -> Nothing
    Nothing -> case find (collide ball) blocks of
                 Just block -> Just $ moveBall (afterCollide ball block)
                 Nothing    -> case find (collide ball) balls of
                                 Just ball' -> Just $ moveBall (afterCollide ball ball')
                                 Nothing    -> case find (collide ball) inkLines of
                                                 Just inkLine -> Just $ moveBall (afterCollide ball inkLine)
                                                 Nothing     -> Just $ moveBall ball

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow (pack "Ink Ball") SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 (35 * 17 - 3) (35 * 17 - 3) }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  gameLoop renderer [ mkBall (70, 70) (Velocity (1, 0.3)) Red
                    , mkBall (220, 220) (Velocity (1, 1))   Blue
                    , mkBall (140, 160) (Velocity (1, 0))   Green
                    , mkBall (120, 120) (Velocity (2, 3.5)) Yellow
                    ] False []

gameLoop :: SDL.Renderer -> [Ball] -> Bool -> [InkLine] -> IO ()
gameLoop renderer balls mouseState ink = do
  events <- SDL.pollEvents
  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
      eventIsMousePress event =
        case SDL.eventPayload event of
          SDL.MouseButtonEvent mouseEvent ->
            SDL.mouseButtonEventButton mouseEvent == SDL.ButtonLeft &&
            SDL.mouseButtonEventMotion mouseEvent == SDL.Pressed
          _ -> False
      mousePressed = any eventIsMousePress events
      eventIsMouseRelease event =
        case SDL.eventPayload event of
          SDL.MouseButtonEvent mouseEvent ->
            SDL.mouseButtonEventButton mouseEvent == SDL.ButtonLeft &&
            SDL.mouseButtonEventMotion mouseEvent == SDL.Released
          _ -> False
      mouseReleased = any eventIsMouseRelease events

  let newMouseState = if mousePressed
                         then True
                         else if mouseReleased
                                 then False
                                 else mouseState
  mPos <- SDL.getAbsoluteMouseLocation
  setColor renderer Gray
  SDL.clear renderer
  drawCells renderer
  board <- readBoard "game.txt" -- TODO: This should be done only once
  let blocks = createBlocks board -- TODO: This should be done only once
      sinks  = createSinks board
      toPoint ::SDL.Point SDL.V2 CInt -> (Float, Float)
      toPoint (SDL.P (SDL.V2 x y)) = (fromIntegral x, fromIntegral y)
      addDotToLine :: InkDot -> InkLine -> InkLine
      addDotToLine dot line = line ++ [dot]
      addDotToNewLine :: [InkLine] -> InkDot -> [InkLine]
      addDotToNewLine lines dot = lines ++ [addDotToLine dot []]
      addDotToLastLine :: [InkLine] -> InkDot -> [InkLine]
      addDotToLastLine [] dot    = [addDotToLine dot []]
      addDotToLastLine lines dot = init lines ++ [addDotToLine dot (last lines)]

  let newInk = if not mouseState && newMouseState
                  then addDotToNewLine ink (mkInkDot $ toPoint mPos)
                  else if newMouseState
                          then addDotToLastLine ink (mkInkDot $ toPoint mPos)
                          else ink
  draw renderer blocks
  draw renderer sinks
  draw renderer balls
  draw renderer newInk
  SDL.present renderer
  threadDelay 10000
  let nextStateBalls = map fromJust (filter isJust (nextState <$> balls <*> [balls] <*> [blocks] <*> [sinks] <*> [newInk]))
      nextStateInk   = map fromJust (filter isJust (nextInkState <$> newInk <*> [balls]))
  unless qPressed (gameLoop renderer nextStateBalls newMouseState nextStateInk)

readBoard :: String -> IO [String]
readBoard filename = do
  contents <- readFile filename
  return (lines contents)

createBlocks :: [String] -> [Block]
createBlocks board = [mkBlock (x, y) color
                        | (row, line) <- enumerate board
                        , (col, cell) <- enumerate line
                        , elem cell ['W', 'R', 'G', 'B', 'Y']
                        , let color = charToColor cell
                              x     = (betweenCells + blockSide) * col
                              y     = (betweenCells + blockSide) * row]
                         where
                           enumerate = zip [0..]
                           charToColor 'W' = White
                           charToColor 'R' = Red
                           charToColor 'G' = Green
                           charToColor 'B' = Blue
                           charToColor 'Y' = Yellow
                           charToColor _   = Gray

createSinks :: [String] -> [Sink]
createSinks board = [mkSink (x, y) color
                       | (row, line) <- enumerate board
                       , (col, cell) <- enumerate line
                       , elem cell ['w', 'r', 'g', 'b', 'y']
                       , let color = charToColor cell
                             x     = (betweenCells + blockSide) * col
                             y     = (betweenCells + blockSide) * row]
                        where
                          enumerate = zip [0..]
                          charToColor 'w' = White
                          charToColor 'r' = Red
                          charToColor 'g' = Green
                          charToColor 'b' = Blue
                          charToColor 'y' = Yellow
                          charToColor _   = Gray
