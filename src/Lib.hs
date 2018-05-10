module Lib where

import qualified SDL
import qualified Foreign.C.Types as CTypes

import GameObjects
import Physics
import Geometry

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
  draw renderer [ Ball (Center (70, 70)  ) (Velocity (1, 0.3)) Red
                , Ball (Center (220, 220)) (Velocity (1, 1))   Blue
                , Ball (Center (140, 160)) (Velocity (1, 0))   Green
                , Ball (Center (120, 120)) (Velocity (2, 3.5)) Yellow
                ]

draw :: SDL.Renderer -> [Ball] -> IO ()
draw renderer balls = do
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
  drawBlocks renderer blocks
  --setColor renderer Red >> fillRectangle renderer (mkRect (35 * 3) (35 * 8) 64 64)
  drawBalls renderer balls
  SDL.present renderer
  threadDelay 10000
  unless qPressed (draw renderer (nextState <$> balls <*> [balls] <*> [blocks]))

-- TODO: Split into drawBall and drawCircle
drawBall :: (Functor m, MonadIO m) => SDL.Renderer -> Ball -> m ()
{-drawBall renderer (Ball (Center (cx, cy)) _ color) = do
  let angles = takeWhile (< 2 * pi) (map (1 / ballRadius *) [0..])
  let xs = map (cx +) (map (ballRadius *) (map cos angles))
  let ys = map (cy +) (map (ballRadius *) (map sin angles))
  let points = zipWith (\x y -> SDL.P (SDL.V2 (round x) (round y))) xs ys
  setColor renderer color
  mapM_ (SDL.drawPoint renderer) points
  -- TODO: Check Mid Point Circle algorithm to better draw the circle-}

toFloat :: CInt -> Float
toFloat = fromIntegral

drawBall renderer (Ball (Center (cx, cy)) _ color) = do
  let side    = 1
      rects = mkRect <$> map (round (cx - ballRadius) + ) (map (side *) [0..round (2 * ballRadius)]) <*> map (round (cy - ballRadius) + ) (map (side *) [0..round (2 * ballRadius)]) <*> [side] <*> [side]
      insideCircle :: SDL.Rectangle CInt -> Bool
      insideCircle (SDL.Rectangle (SDL.P (SDL.V2 x y)) _) = distance (Point (toFloat x, toFloat y)) (Point (cx, cy)) < ballRadius

  setColor renderer color
  mapM_ (fillRectangle renderer) (filter insideCircle rects)

drawBalls :: (Functor m, MonadIO m) => SDL.Renderer -> [Ball] -> m ()
drawBalls renderer = mapM_ (drawBall renderer)

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

drawBlocks :: MonadIO m => SDL.Renderer -> [Block] -> m ()
drawBlocks renderer = mapM_ (drawBlock renderer)

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
