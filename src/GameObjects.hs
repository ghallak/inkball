module GameObjects where

import Geometry

data Color     = White
               | Red
               | Blue
               | Green
               | Yellow
               | Gray
               | Black
               | DarkGray
               deriving (Show, Eq)
data BlockSide = TopSide
               | BottomSide
               | RightSide
               | LeftSide
               deriving (Show, Eq)

newtype Velocity = Velocity (Float, Float) deriving (Show, Eq)

data Block = Block (Square Float) Color               deriving (Show, Eq)
data Ball  = Ball (Circle Float) Velocity Color       deriving (Show, Eq)
data Sink  = Sink (Square Float) (Circle Float) Color deriving (Show, Eq)

-- TODO: Consider removing InkDot and changing InkLine to `InkLine [Circle Float]`
newtype InkDot  = InkDot (Circle Float)
newtype InkLine = InkLine [InkDot]

mkBlock :: (Float, Float) -> Color -> Block
mkBlock (x, y) color = Block (Square (Point (x, y)) blockSide) color

mkBall :: (Float, Float) -> Velocity -> Color -> Ball
mkBall (x, y) v color = Ball (Circle (Point (x, y)) ballRadius) v color

mkSink :: (Float, Float) -> Color -> Sink
mkSink (x, y) color = Sink (Square (Point (x, y)) sinkSide) (Circle (Point (x + sinkSide / 2, y + sinkSide / 2)) sinkHoleRadius) color

mkInkDot :: (Float, Float) -> InkDot
mkInkDot (x, y) = InkDot (Circle (Point (x, y)) inkRadius)

betweenCells   :: Float
betweenCells   = 3

inkRadius      :: Float
inkRadius      = 8

ballRadius     :: Float
ballRadius     = 16

blockSide      :: Float
blockSide      = 32

sinkHoleRadius :: Float
sinkHoleRadius = 24

sinkSide       :: Float
sinkSide       = 64
