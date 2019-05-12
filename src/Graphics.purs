module Graphics
  ( drawBlock
  , drawBall
  , drawSink
  , drawInkLine
  ) where

import Prelude
import Data.Array (length, head, last, init, slice, zipWith, tail, zip)
import Data.Maybe (fromMaybe)
import Data.Tuple
import Effect (Effect, foreachE)
import Effect.Console (log)
import Graphics.Canvas as C
import Math (pi)

import GameObjects (Color(..), InkDot(..), Ball, Block, InkLine, Sink, mkInkDot)
import Geometry (Square, Circle)

drawInkLine :: C.Context2D -> InkLine -> Effect Unit
drawInkLine ctx inkLine = do
  let len = length inkLine

  if len > 3
    then do
      let uselessPoint = mkInkDot { x: 500, y: 500 } -- TODO: delete later
          firstPoint = inkDotToPoint <<< fromMaybe uselessPoint $ head inkLine
          lastPoint = inkDotToPoint <<< fromMaybe uselessPoint $ last inkLine
          beforeLastPoint = inkDotToPoint <<< fromMaybe uselessPoint <<< last <<< fromMaybe [] $ init inkLine
          middle = map inkDotToPoint (slice 1 (length inkLine - 3) inkLine)
          midPoints = zipWith (\p q -> (p + q) * { x: 0.5, y: 0.5 }) middle (fromMaybe [] (tail middle))

      C.beginPath ctx
      C.moveTo ctx firstPoint.x firstPoint.y
      foreachE (zip middle midPoints) (C.quadraticCurveTo ctx <<< f)
      C.quadraticCurveTo ctx { cpx: beforeLastPoint.x, cpy: beforeLastPoint.y, x: lastPoint.x, y: lastPoint.y }
      C.stroke ctx
    else pure unit

  where
    inkDotToPoint :: InkDot -> { x :: Number, y :: Number }
    inkDotToPoint (InkDot inkDot) = inkDot.center

    f :: Tuple { x :: Number, y :: Number } { x :: Number, y :: Number } -> C.QuadraticCurve
    f (Tuple p q) = { cpx: p.x, cpy: p.y, x: q.x, y: q.y }

drawInkDot :: C.Context2D -> InkDot -> Effect Unit
drawInkDot ctx (InkDot inkDot) = drawCircle ctx inkDot Black

drawBlock :: C.Context2D -> Block -> Effect Unit
drawBlock ctx block = drawSquare ctx block.square block.color

drawBall :: C.Context2D -> Ball -> Effect Unit
drawBall ctx ball = drawCircle ctx ball.circle ball.color

drawSink :: C.Context2D -> Sink -> Effect Unit
drawSink ctx sink = do
  drawSquare ctx sink.square sink.color
  drawCircle ctx sink.circle Black

drawCircle :: C.Context2D -> Circle -> Color -> Effect Unit
drawCircle ctx circle color = do
  let cx = circle.center.x
      cy = circle.center.y
      radius = circle.radius
      clr = show color
  C.setFillStyle ctx clr
  C.beginPath ctx
  C.arc ctx { x: cx, y: cy, radius: radius, start: 0.0, end: 2.0 * pi }
  C.fill ctx

drawSquare :: C.Context2D -> Square -> Color -> Effect Unit
drawSquare ctx square color = do
  let side = square.side
      topLeft = square.topLeft
      clr = show color
  C.setFillStyle ctx clr
  C.fillRect ctx { x: topLeft.x, y: topLeft.y, width: side, height: side }
