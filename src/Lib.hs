module Lib where

import qualified SDL
import qualified Foreign.C.Types as CTypes

import GameObjects
import Physics

import Control.Monad          (unless)
import Data.Text              (pack)
import Data.List              (find)
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types        (CInt)
import SDL                    (($=))
import System.IO              (readFile)
import Control.Concurrent     (threadDelay)



nextState :: Ball -> [Block] -> Ball
nextState ball blocks = case find (collide ball) blocks of
                          Just block -> moveBall (afterCollide ball block)
                          Nothing    -> moveBall ball

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow (pack "My SDL Application") SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 (35 * 17 - 3) (35 * 17 - 3) }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  draw renderer (Ball (Center (70, 70)) (Velocity (1, 0.3)) Red)
  --draw renderer (Ball (Center (70, 90)) Red (Speed 1) (Direction (1, 1)))
  --draw renderer (Ball (Center (70, 70)) Red (Speed 1) (Direction (1, 0)))
  --draw renderer (Ball (Center (70, 70)) Red (Speed 1) (Direction (0, 1)))
  --draw renderer (Ball (Center (70, 70)) Red (Speed 1) (Direction (1, 1)))

draw :: SDL.Renderer -> Ball -> IO ()
draw renderer ball = do
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
  mapM_ (drawBlock renderer) blocks
  --setColor renderer Red >> fillRectangle renderer (mkRect (35 * 3) (35 * 8) 64 64)
  drawBall renderer ball
  SDL.present renderer
  threadDelay 3000
  unless qPressed (draw renderer (nextState ball blocks))

-- TODO: Split into drawBall and drawCircle
drawBall :: (Functor m, MonadIO m) => SDL.Renderer -> Ball -> m ()
drawBall renderer (Ball (Center (cx, cy)) _ color) = do
  let angles = takeWhile (< 2 * pi) (map (1 / ballRadius *) [0..])
  let xs = map (cx +) (map (ballRadius *) (map cos angles))
  let ys = map (cy +) (map (ballRadius *) (map sin angles))
  let points = zipWith (\x y -> SDL.P (SDL.V2 (round x) (round y))) xs ys
  setColor renderer color
  mapM_ (SDL.drawPoint renderer) points
  -- TODO: Check Mid Point Circle algorithm to better draw the circle

readGame :: String -> IO String
readGame filename = do
  contents <- readFile filename
  return (concat $ lines contents)

readBoard :: String -> IO [String]
readBoard filename = do
  contents <- readFile filename
  return (lines contents)

createBlocks :: [String] -> [Block]
createBlocks board = [Block (TopLeft (x, y)) color
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

drawCells :: MonadIO m => SDL.Renderer -> m ()
drawCells renderer = do
  let rects = mkRect <$> (map (35*) [0..16]) <*> (map (35*) [0..16]) <*> [32] <*> [32]
  setColor renderer DarkGray
  mapM_ (fillRectangle renderer) rects

drawBlock :: MonadIO m => SDL.Renderer -> Block -> m ()
drawBlock renderer (Block (TopLeft (t, l)) color) = setColor renderer color >> fillRectangle renderer (mkSquare (CTypes.CInt $ fromIntegral $ round t) (CTypes.CInt $ fromIntegral $ round l) (CTypes.CInt $ fromIntegral $ round blockSide))

mkSquare :: a -> a -> a -> SDL.Rectangle a
mkSquare x y side = mkRect x y side side

mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h

fillRectangle :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
fillRectangle r s = SDL.fillRect r (Just s)

setColor :: (MonadIO m) => SDL.Renderer -> Color -> m ()
setColor r White    = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Red      = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
setColor r Green    = SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
setColor r Blue     = SDL.rendererDrawColor r $= SDL.V4 0 0 maxBound maxBound
setColor r Yellow   = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound
setColor r Gray     = SDL.rendererDrawColor r $= SDL.V4 204 204 204 maxBound
setColor r DarkGray = SDL.rendererDrawColor r $= SDL.V4 191 191 191 maxBound
