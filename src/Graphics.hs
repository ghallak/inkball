module Graphics where

import qualified SDL

import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types        (CInt)
import SDL                    (($=))

import GameObjects
import Geometry

class Draw a where
  draw :: MonadIO m => SDL.Renderer -> a -> m ()

instance Draw a => Draw [a] where
  draw r [] = return ()
  draw r (x : xs) = draw r x >> draw r xs

instance Draw Block where
  draw r (Block square color) = drawSquare r square color

instance Draw Ball where
  draw r (Ball circle _ color) = drawCircle r circle color

instance Draw Sink where
  draw r (Sink square circle color) = drawSquare r square color >> drawCircle r circle Black

instance Draw InkDot where
  draw r (InkDot circle) = drawCircle r circle Black

drawCells :: MonadIO m => SDL.Renderer -> m ()
drawCells renderer = do
  let rects = mkRect <$> (map (35*) [0..16]) <*> (map (35*) [0..16]) <*> [32] <*> [32]
  setColor renderer DarkGray
  mapM_ (fillRectangle renderer) rects

drawCircle :: MonadIO m => SDL.Renderer -> Circle Float -> Color -> m ()
drawCircle r (Circle (Point (cx, cy)) radius) color = do
  setColor r color
  -- Midpoint Circle Algorithm
  -- TODO: The circles are not round enough
  drawCircle' (radius - 1) 0 1 1 (1 - radius * 2)
    where
      drawCircle' x y dx dy err
        | x < y     = return ()
        | otherwise = do
            SDL.drawLine r (toSDLPoint $ Point (cx + x, cy + y)) (toSDLPoint $ Point (cx - x, cy + y))
            SDL.drawLine r (toSDLPoint $ Point (cx + y, cy + x)) (toSDLPoint $ Point (cx - y, cy + x))
            SDL.drawLine r (toSDLPoint $ Point (cx - x, cy - y)) (toSDLPoint $ Point (cx + x, cy - y))
            SDL.drawLine r (toSDLPoint $ Point (cx - y, cy - x)) (toSDLPoint $ Point (cx + y, cy - x))

            if err <= 0
               then drawCircle' x (y + 1) dx (dy + 2) (err + dy)
               else drawCircle' (x - 1) y (dx + 2) dy (err + dx - radius * 2)

drawSquare :: MonadIO m => SDL.Renderer -> Square Float -> Color -> m ()
drawSquare r square color = setColor r color >> fillRectangle r (toRect square)

toRect :: Square Float -> SDL.Rectangle CInt
toRect (Square (Point (x, y)) side) =
  mkRect (toCInt x) (toCInt y) (toCInt side) (toCInt side)
  where
    toCInt :: Float -> CInt
    toCInt x = fromIntegral $ round x

toSDLPoint :: Point Float -> SDL.Point SDL.V2 CInt
toSDLPoint (Point (x, y)) = SDL.P (SDL.V2 (toCInt x) (toCInt y))
  where
    toCInt :: Float -> CInt
    toCInt x = fromIntegral $ round x

mkRect :: CInt -> CInt -> CInt -> CInt -> SDL.Rectangle CInt
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
setColor r Black    = SDL.rendererDrawColor r $= SDL.V4 0 0 0 maxBound
setColor r Gray     = SDL.rendererDrawColor r $= SDL.V4 204 204 204 maxBound
setColor r DarkGray = SDL.rendererDrawColor r $= SDL.V4 191 191 191 maxBound
