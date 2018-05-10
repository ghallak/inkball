module GameObjects where

import Geometry

data Color     = White
               | Red
               | Blue
               | Green
               | Yellow
               | Gray
               | DarkGray
               deriving (Show, Eq)
data BlockSide = TopSide
               | BottomSide
               | RightSide
               | LeftSide
               deriving (Show, Eq)

newtype Velocity = Velocity (Float, Float) deriving (Show, Eq)

data Block = Block (Square Float) Color         deriving (Show, Eq)
data Ball  = Ball (Circle Float) Velocity Color deriving (Show, Eq)

mkBlock :: (Float, Float) -> Color -> Block
mkBlock (x, y) color = Block (Square (Point (x, y)) blockSide) color

mkBall :: (Float, Float) -> Velocity -> Color -> Ball
mkBall (x, y) v color = Ball (Circle (Point (x, y)) ballRadius) v color

betweenCells :: Float
betweenCells = 3

ballRadius   :: Float
ballRadius   = 16

blockSide    :: Float
blockSide    = 32
