module Geometry
  ( Point (..)
  , Vec (..)
  , Segment (..)
  , Circle (..)
  , Square (..)
  , normalize
  , dot
  , pointToSegEnds
  , circleIntersectSeg
  , circlesIntersect
  , multiplyByScalar
  , vecFromPoint
  ) where

import Control.Semigroupoid ((<<<))
import Data.BooleanAlgebra ((||))
import Data.EuclideanRing ((/))
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Ord (abs, (<))
import Data.Ring (class Ring, (-))
import Data.Semiring (class Semiring, (*), (+))
import Math (pow, sqrt)

newtype Point = Point
  { x :: Number
  , y :: Number
  }

instance semiringPoint :: Semiring Point where
  add (Point p) (Point q) = Point {x: p.x + q.x, y: p.y + q.y}
  mul (Point p) (Point q) = Point {x: p.x * q.x, y: p.y * q.y}
  zero = Point {x: 0.0, y: 0.0}
  one = Point {x: 1.0, y: 1.0}

instance ringPoint :: Ring Point where
  sub (Point p) (Point q) = Point {x: p.x - q.x, y: p.y - q.y}

derive instance newtypePoint :: Newtype Point _

newtype Vec = Vec
  { x :: Number
  , y :: Number
  }

derive instance newtypeVec :: Newtype Vec _

newtype Segment = Segment
  { p :: Point
  , q :: Point
  }

newtype Circle = Circle
  { center :: Point
  , radius :: Number
  }

newtype Square = Square
  { topLeft :: Point
  , side    :: Number
  }

infixl 4 almostEqualTo as ~==

almostEqualTo :: Number -> Number -> Boolean
almostEqualTo x y = abs (x - y) < (1e-6)

infixl 4 almostLessThanOrEqualTo as ~<=

almostLessThanOrEqualTo :: Number -> Number -> Boolean
almostLessThanOrEqualTo x y = x ~== y || x < y

dot :: Vec -> Vec -> Number
dot (Vec v) (Vec v') = v.x * v'.x + v.y * v'.y

magnitude :: Vec -> Number
magnitude (Vec v) = sqrt (pow v.x 2.0 + pow v.y 2.0)

normalize :: Vec -> Vec
normalize (Vec v) =
  let mag = magnitude (Vec v)
   in Vec {x: v.x / mag, y: v.y / mag}

distance :: Point -> Point -> Number
distance (Point p) (Point q) = sqrt (pow (p.x - q.x) 2.0 + pow (p.y - q.y) 2.0)

multiplyByScalar :: Vec -> Number -> Vec
multiplyByScalar (Vec v) s = Vec {x: v.x * s, y: v.y * s}

vecFromPoint :: Point -> Vec
vecFromPoint = wrap <<< unwrap

pointPlusVec :: Point -> Vec -> Point
pointPlusVec (Point p) (Vec v) = Point {x: p.x + v.x, y: p.y + v.y}

circleIntersectSeg :: Circle -> Segment -> Boolean
circleIntersectSeg (Circle circle) seg =
  let closestPoint = closestPointOnSeg circle.center seg
   in distance circle.center closestPoint ~<= circle.radius

circlesIntersect :: Circle -> Circle -> Boolean
circlesIntersect (Circle circleA) (Circle circleB) =
  let distBetweenCenters = distance circleA.center circleB.center
      sumRadiuses = circleA.radius + circleB.radius
   in distBetweenCenters ~<= sumRadiuses

pointInCircle :: Circle -> Point -> Boolean
pointInCircle (Circle circle) p = distance p circle.center ~<= circle.radius

squareInsideCircle :: Square -> Circle -> Boolean
squareInsideCircle square circle = all (pointInCircle circle) (vertices square)

vertices :: Square -> Array Point
vertices (Square sq) =
  let topLeftVertex = sq.topLeft
      topRightVertex = sq.topLeft + Point {x: sq.side, y: 0.0}
      bottomLeftVertex = sq.topLeft + Point {x: 0.0, y: sq.side}
      bottomRightVertex = sq.topLeft + Point {x: sq.side, y: sq.side}
   in [topLeftVertex, topRightVertex, bottomLeftVertex, bottomRightVertex]

pointOnSeg :: Point -> Segment -> Boolean
pointOnSeg point (Segment seg) =
  let pointToP = distance point seg.p
      pointToQ = distance point seg.q
      segLength = distance seg.p seg.q
   in pointToP + pointToQ ~== segLength

pointToSegEnds :: Point -> Segment -> Number
pointToSegEnds point (Segment seg) = distance seg.p point + distance seg.q point

closestPointOnSeg :: Point -> Segment -> Point
closestPointOnSeg point (Segment seg) =
  let closestEndOnSeg = if distance seg.p point < distance seg.q point
                          then seg.p
                          else seg.q
   in case projectPointOnSeg point (Segment seg) of
           Just p  -> p
           Nothing -> closestEndOnSeg

projectPointOnSeg :: Point -> Segment -> Maybe Point
projectPointOnSeg c (Segment seg) =
  let pq = vecFromPoint (seg.q - seg.p)
      pc = vecFromPoint (c - seg.p)
      p  = pointPlusVec seg.p (multiplyByScalar pq (dot pq pc / dot pq pq))
   in if pointOnSeg p (Segment seg)
        then Just p
        else Nothing
