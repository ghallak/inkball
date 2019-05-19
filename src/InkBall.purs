module InkBall
  ( main
  ) where

import Prelude
import Effect (Effect)
import Signal (runSignal, (~>))

import InkBall.Graphics (drawForeground, drawBackground)
import InkBall.Signals (mousePosEveryFrame, gameSignal)

main :: Effect Unit
main = do
  drawBackground

  mousePosOnFrame <- mousePosEveryFrame
  runSignal $ gameSignal mousePosOnFrame ~> drawForeground
  pure unit
