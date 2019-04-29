module GameObjects
  ( Color (..)
  , BlockSide (..)
  , Velocity
  , InkDot (..)
  , Block
  , Ball
  , Sink
  , InkLine
  , mkBlock
  , mkBall
  , mkSink
  , mkInkDot
  , blockSide
  , betweenCells
  ) where

import Data.EuclideanRing ((/))
import Data.Semiring ((+))
import Data.Show (class Show)

import Geometry (Circle, Square, Vec)

data Color
  = White
  | Red
  | Blue
  | Green
  | Yellow
  | Gray
  | Black
  | DarkGray

instance showColor :: Show Color where
  show White = "white"
  show Red = "Red"
  show Blue = "Blue"
  show Green = "Green"
  show Yellow = "Yellow"
  show Gray = "Gray"
  show Black = "Black"
  show DarkGray = "DarkGray"

data BlockSide
  = TopSide
  | BottomSide
  | RightSide
  | LeftSide

type Velocity = Vec

type Block =
  { square :: Square
  , color  :: Color
  }

type Ball =
  { circle   :: Circle
  , velocity :: Velocity
  , color    :: Color
  }

type Sink =
  { square :: Square
  , circle :: Circle
  , color  :: Color
  }

newtype InkDot  = InkDot Circle

type InkLine = Array InkDot

mkBlock :: {x :: Number, y :: Number} -> Color -> Block
mkBlock xy color =
  { square:
      { topLeft: {x: xy.x, y: xy.y}
      , side: blockSide
      }
  , color: color
  }

mkBall :: {x :: Number, y :: Number} -> Velocity -> Color -> Ball
mkBall xy vel color =
  { circle:
      { center: {x: xy.x, y: xy.y}
      , radius: ballRadius
      }
  , velocity: vel
  , color: color
  }

mkSink :: {x :: Number, y :: Number} -> Color -> Sink
mkSink xy color =
  { square:
      { topLeft: {x: xy.x, y: xy.y}
      , side: sinkSide
      }
  , circle:
      { center: {x: xy.x + sinkSide / 2.0, y: xy.y + sinkSide / 2.0}
      , radius: sinkHoleRadius
      }
  , color: color
  }

mkInkDot :: {x :: Number, y :: Number} -> InkDot
mkInkDot xy = InkDot
  { center: {x: xy.x, y: xy.y}
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
