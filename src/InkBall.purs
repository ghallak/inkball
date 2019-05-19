module InkBall
  ( main
  ) where

import Prelude
import Effect (Effect)
import Signal (runSignal, (~>))

import InkBall.Graphics (drawForeground, drawBackground)
import InkBall.Signals (mousePosEveryFrame, gameSignal, launchBallSignal)

main :: Effect Unit
main = do
  drawBackground

  mousePosOnFrame <- mousePosEveryFrame
  launchBall <- launchBallSignal
  runSignal $ gameSignal (launchBall <> mousePosOnFrame) ~> drawForeground
  pure unit
