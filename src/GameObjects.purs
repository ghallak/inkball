module GameObjects
  ( Color (..)
  , BlockSide (..)
  , Velocity (..)
  , Block (..)
  , Ball (..)
  , Sink (..)
  , InkDot (..)
  , InkLine
  , class Circular
  , getCircle
  , mkBlock
  , mkBall
  , mkSink
  , mkInkDot
  , blockSide
  , betweenCells
  ) where

import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Semiring ((+))

import Geometry (Square (..), Point (..), Circle (..), Vec)

data Color
  = White
  | Red
  | Blue
  | Green
  | Yellow
  | Gray
  | Black
  | DarkGray

data BlockSide
  = TopSide
  | BottomSide
  | RightSide
  | LeftSide

newtype Velocity = Velocity Vec

newtype Block = Block
  { square :: Square
  , color  :: Color
  }

newtype Ball = Ball
  { circle   :: Circle
  , velocity :: Velocity
  , color    :: Color
  }

newtype Sink = Sink
  { square :: Square
  , circle :: Circle
  , color  :: Color
  }

newtype InkDot  = InkDot Circle

type InkLine = Array InkDot

class Circular a where
  getCircle :: a -> Circle

instance circularBall :: Circular Ball where
  getCircle (Ball ball) = ball.circle

instance circularSink :: Circular Sink where
  getCircle (Sink sink) = sink.circle

instance circularInkDot :: Circular InkDot where
  getCircle (InkDot circle) = circle

mkBlock :: {x :: Number, y :: Number} -> Color -> Block
mkBlock xy color = Block
  { square: Square
      { topLeft: Point {x: xy.x, y: xy.y}
      , side: blockSide
      }
  , color: color
  }

mkBall :: {x :: Number, y :: Number} -> Velocity -> Color -> Ball
mkBall xy vel color = Ball
  { circle: Circle
      { center: Point {x: xy.x, y: xy.y}
      , radius: ballRadius
      }
  , velocity: vel
  , color: color
  }

mkSink :: {x :: Number, y :: Number} -> Color -> Sink
mkSink xy color = Sink
  { square: Square
      { topLeft: Point {x: xy.x, y: xy.y}
      , side: sinkSide
      }
  , circle: Circle
      { center: Point {x: xy.x + sinkSide / 2.0, y: xy.y + sinkSide / 2.0}
      , radius: sinkHoleRadius
      }
  , color: color
  }

mkInkDot :: {x :: Number, y :: Number} -> InkDot
mkInkDot xy = InkDot $ Circle
  { center: Point {x: xy.x, y: xy.y}
  , radius: inkRadius
  }

betweenCells :: Number
betweenCells = 3.0

inkRadius :: Number
inkRadius = 6.0

ballRadius :: Number
ballRadius = 16.0

sinkHoleRadius :: Number
sinkHoleRadius = 24.0

blockSide :: Number
blockSide = 32.0

sinkSide :: Number
sinkSide = 64.0
