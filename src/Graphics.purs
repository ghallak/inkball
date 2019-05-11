module Graphics
  ( drawBlock
  , drawBall
  , drawSink
  , drawInkLine
  ) where

import Prelude
import Effect (Effect, foreachE)
import Graphics.Canvas as C
import Math (pi)

import GameObjects (Color(..), InkDot(..), Ball, Block, InkLine, Sink)
import Geometry (Square, Circle)

drawInkLine :: C.Context2D -> InkLine -> Effect Unit
drawInkLine ctx inkLine = foreachE inkLine (drawInkDot ctx)

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
