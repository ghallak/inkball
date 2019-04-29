module Physics
  ( moveBall
  ) where

import Prelude

import GameObjects (Ball)

moveBall :: Ball -> Ball
moveBall ball =
  ball
    { circle
      { center
        { x = ball.circle.center.x + ball.velocity.x
        , y = ball.circle.center.y + ball.velocity.y
        }
      }
    }
