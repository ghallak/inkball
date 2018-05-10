module Lib where

import qualified SDL
import qualified Foreign.C.Types as CTypes

import GameObjects
import Physics
import Geometry
import Graphics

import Control.Monad          (unless)
import Data.Text              (pack)
import Data.List              (find)
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types        (CInt)
import SDL                    (($=))
import System.IO              (readFile)
import Control.Concurrent     (threadDelay)



nextState :: Ball -> [Ball] -> [Block] -> Ball
nextState ball balls blocks = case find (collide ball) blocks of
                                Just block -> moveBall (afterCollide ball block)
                                Nothing    -> case find (collide ball) balls of
                                                Just ball' -> moveBall (afterCollide ball ball')
                                                Nothing    -> moveBall ball

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow (pack "Ink Ball") SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 (35 * 17 - 3) (35 * 17 - 3) }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  gameLoop renderer [ mkBall (70, 70) (Velocity (1, 0.3)) Red
                    , mkBall (220, 220) (Velocity (1, 1))   Blue
                    , mkBall (140, 160) (Velocity (1, 0))   Green
                    , mkBall (120, 120) (Velocity (2, 3.5)) Yellow
                    ]

gameLoop :: SDL.Renderer -> [Ball] -> IO ()
gameLoop renderer balls = do
  events <- SDL.pollEvents
  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  setColor renderer Gray
  SDL.clear renderer
  drawCells renderer
  board <- readBoard "game.txt" -- TODO: This should be done only once
  let blocks = createBlocks board -- TODO: This should be done only once
  draw renderer blocks
  --setColor renderer Red >> fillRectangle renderer (mkRect (35 * 3) (35 * 8) 64 64)
  draw renderer balls
  SDL.present renderer
  threadDelay 300
  unless qPressed (gameLoop renderer (nextState <$> balls <*> [balls] <*> [blocks]))

readBoard :: String -> IO [String]
readBoard filename = do
  contents <- readFile filename
  return (lines contents)

createBlocks :: [String] -> [Block]
createBlocks board = [mkBlock (x, y) color
                       | (row, line) <- enumerate board
                       , (col, cell) <- enumerate line
                       , cell /= '.'
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
