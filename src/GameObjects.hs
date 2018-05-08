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

newtype Center   = Center (Float, Float)   deriving (Show, Eq)
newtype TopLeft  = TopLeft (Float, Float)  deriving (Show, Eq)
newtype Velocity = Velocity (Float, Float) deriving (Show, Eq)

class ToPoint a where
  toPoint :: a -> Point Float

instance ToPoint Center where
  toPoint (Center (x, y)) = Point (x, y)

instance ToPoint TopLeft where
  toPoint (TopLeft (x, y)) = Point (x, y)

class ToVec a where
  toVec :: a -> Vec Float

instance ToVec Velocity where
  toVec (Velocity (x, y)) = Vec (x, y)

data Block = Block TopLeft Color        deriving (Show, Eq)
data Ball  = Ball Center Velocity Color deriving (Show, Eq)

betweenCells :: Float
betweenCells = 3

ballRadius   :: Float
ballRadius   = 16

blockSide    :: Float
blockSide    = 32
