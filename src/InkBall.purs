module InkBall
  ( main
  ) where

import Prelude
import Effect (Effect)
import Signal (runSignal, (~>))

import InkBall.Boards (randomBoard)
import InkBall.Graphics (drawForeground, drawBackground)
import InkBall.Signals (mousePosEveryFrame, gameSignal, launchBallSignal)

main :: Effect Unit
main = do
  board <- randomBoard
  drawBackground board
  mousePosOnFrame <- mousePosEveryFrame
  launchBall <- launchBallSignal board
  runSignal $ gameSignal board (launchBall <> mousePosOnFrame) ~> drawForeground
  pure unit
