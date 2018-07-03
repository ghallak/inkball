module Lib where

import qualified SDL
import qualified Foreign.C.Types as CTypes

import Data.Bool              (bool)
import Data.Maybe             (isJust, fromJust)
import Control.Monad          (unless)
import Data.Text              (pack)
import Data.List              (find)
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types        (CInt)
import SDL                    (($=))
import System.IO              (readFile)
import Control.Concurrent     (MVar, newMVar, readMVar, forkIO, threadDelay)

import GameObjects (InkLine, InkDot, Ball, Sink, Block, Color (..), Velocity (..), mkBall, mkInkDot, betweenCells, mkBlock, blockSide, mkSink)
import Physics (collide, afterCollide, moveBall)
import Graphics (draw, drawCells, setColor)
import Input (inputHandler)

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
  let blocks = createBlocks board
      sinks  = createSinks board
  mouseHeld <- newMVar False
  quitPressed <- newMVar False
  forkIO $ inputHandler mouseHeld quitPressed
  gameLoop renderer blocks sinks [ mkBall (70, 70)   (Velocity (1, 0.3)) Red
                                 , mkBall (220, 220) (Velocity (1, 1))   Blue
                                 , mkBall (140, 160) (Velocity (1, 0))   Green
                                 , mkBall (120, 120) (Velocity (2, 3.5)) Yellow
                                 ] [] mouseHeld quitPressed False

renderGameObjects :: SDL.Renderer -> [Block] -> [Sink] -> [Ball] -> [InkLine] -> IO ()
renderGameObjects r blocks sinks balls inkLines = do
  setColor r Gray
  SDL.clear r
  drawCells r
  draw r blocks
  draw r sinks
  draw r balls
  draw r inkLines
  SDL.present r

gameLoop :: SDL.Renderer -> [Block] -> [Sink] -> [Ball] -> [InkLine] -> MVar Bool -> MVar Bool -> Bool -> IO ()
gameLoop renderer blocks sinks balls inkLines mouseHeld quitPressedMVar lastMouseState = do
  mouseState <- readMVar mouseHeld
  mPos <- SDL.getAbsoluteMouseLocation
  let toPoint ::SDL.Point SDL.V2 CInt -> (Float, Float)
      toPoint (SDL.P (SDL.V2 x y)) = (fromIntegral x, fromIntegral y)
      addDotToLine :: InkDot -> InkLine -> InkLine
      addDotToLine dot line = line ++ [dot]
      addDotToNewLine :: [InkLine] -> InkDot -> [InkLine]
      addDotToNewLine lines dot = lines ++ [addDotToLine dot []]
      addDotToLastLine :: [InkLine] -> InkDot -> [InkLine]
      addDotToLastLine [] dot    = [addDotToLine dot []]
      addDotToLastLine lines dot = init lines ++ [addDotToLine dot (last lines)]

  let newInkLines = if not lastMouseState && mouseState
                       then addDotToNewLine inkLines (mkInkDot $ toPoint mPos)
                       else if mouseState
                               then addDotToLastLine inkLines (mkInkDot $ toPoint mPos)
                               else inkLines
  renderGameObjects renderer blocks sinks balls newInkLines
  threadDelay 10000
  let nextStateBalls = map fromJust (filter isJust (nextState <$> balls <*> [balls] <*> [blocks] <*> [sinks] <*> [newInkLines]))
      nextStateInk   = map fromJust (filter isJust (nextInkState <$> newInkLines <*> [balls]))
  quitPressed <- readMVar quitPressedMVar
  unless quitPressed $ gameLoop renderer blocks sinks nextStateBalls nextStateInk mouseHeld quitPressedMVar mouseState

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

board :: [String]
board = [ "WWWWWWWWWWWWWWWWW"
        , "W...............W"
        , "W...............W"
        , "W...............W"
        , "W...........r#..W"
        , "W...........##..W"
        , "W...W...........W"
        , "W...............W"
        , "W...............W"
        , "W........W......W"
        , "W...............W"
        , "W.........g#....W"
        , "W.........##....W"
        , "W...............W"
        , "W...............W"
        , "W...............W"
        , "WWWWWWWWWWWWWWWWW"]
