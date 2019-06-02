module InkBall.Geometry
  ( Point
  , Vec
  , Segment
  , Circle
  , Square
  , normalize
  , dot
  , pointToSegEnds
  , circleIntersectSeg
  , circlesIntersect
  , multiplyByScalar
  , circlesTouchingPoint
  , magnitude
  ) where

import Prelude

import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.List (List(..), head, last, length, slice, zip, zipWith, snoc, (:))
import Data.Tuple (Tuple(..))
import Math (abs, pow, sqrt)

type Point =
  { x :: Number
  , y :: Number
  }

type Vec =
  { x :: Number
  , y :: Number
  }

type Segment =
  { p :: Point
  , q :: Point
  }

type Circle =
  { center :: Point
  , radius :: Number
  }

type Square =
  { topLeft :: Point
  , side    :: Number
  }

type QuadraticCurve =
  { start   :: Point
  , end     :: Point
  , control :: Point
  }

infixl 4 almostEqualTo as ~==

almostEqualTo :: Number -> Number -> Boolean
almostEqualTo x y = abs (x - y) < (1e-6)

infixl 4 almostLessThanOrEqualTo as ~<=

almostLessThanOrEqualTo :: Number -> Number -> Boolean
almostLessThanOrEqualTo x y = x ~== y || x < y

dot :: Vec -> Vec -> Number
dot v v' = v.x * v'.x + v.y * v'.y

magnitude :: Vec -> Number
magnitude v = sqrt (pow v.x 2.0 + pow v.y 2.0)

normalize :: Vec -> Vec
normalize v =
  let mag = magnitude v
   in {x: v.x / mag, y: v.y / mag}

distance :: Point -> Point -> Number
distance p q = sqrt (pow (p.x - q.x) 2.0 + pow (p.y - q.y) 2.0)

middlePoint :: Point -> Point -> Point
middlePoint p q = (p + q) * { x: 0.5, y: 0.5 }

multiplyByScalar :: Vec -> Number -> Vec
multiplyByScalar v s = {x: v.x * s, y: v.y * s}

pointPlusVec :: Point -> Vec -> Point
pointPlusVec p v = {x: p.x + v.x, y: p.y + v.y}

circleIntersectSeg :: Circle -> Segment -> Boolean
circleIntersectSeg circle seg =
  let closestPoint = closestPointOnSeg circle.center seg
   in distance circle.center closestPoint ~<= circle.radius

circlesIntersect :: Circle -> Circle -> Boolean
circlesIntersect circleA circleB =
  let distBetweenCenters = distance circleA.center circleB.center
      sumRadiuses = circleA.radius + circleB.radius
   in distBetweenCenters ~<= sumRadiuses

pointInCircle :: Circle -> Point -> Boolean
pointInCircle circle p = distance p circle.center ~<= circle.radius

squareInsideCircle :: Square -> Circle -> Boolean
squareInsideCircle square circle = all (pointInCircle circle) (vertices square)

vertices :: Square -> Array Point
vertices sq =
  let topLeftVertex = sq.topLeft
      topRightVertex = sq.topLeft + {x: sq.side, y: 0.0}
      bottomLeftVertex = sq.topLeft + {x: 0.0, y: sq.side}
      bottomRightVertex = sq.topLeft + {x: sq.side, y: sq.side}
   in [topLeftVertex, topRightVertex, bottomLeftVertex, bottomRightVertex]

pointOnSeg :: Point -> Segment -> Boolean
pointOnSeg point seg =
  let pointToP = distance point seg.p
      pointToQ = distance point seg.q
      segLength = distance seg.p seg.q
   in pointToP + pointToQ ~== segLength

pointToSegEnds :: Point -> Segment -> Number
pointToSegEnds point seg = distance seg.p point + distance seg.q point

closestPointOnSeg :: Point -> Segment -> Point
closestPointOnSeg point seg =
  let closestEndOnSeg = if distance seg.p point < distance seg.q point
                          then seg.p
                          else seg.q
   in case projectPointOnSeg point seg of
        Just p  -> p
        Nothing -> closestEndOnSeg

projectPointOnSeg :: Point -> Segment -> Maybe Point
projectPointOnSeg c seg =
  let pq = seg.q - seg.p
      pc = c - seg.p
      p  = pointPlusVec seg.p (multiplyByScalar pq (dot pq pc / dot pq pq))
   in if pointOnSeg p seg
        then Just p
        else Nothing

circlesTouchingPoint :: Circle -> Circle -> Point
circlesTouchingPoint circle circle' =
  let centersDiff = circle'.center - circle.center
      radiusesSum = circle'.radius + circle.radius
      point = { x: circle.radius / radiusesSum, y : circle.radius / radiusesSum }
   in circle.center + (centersDiff * point)

-- start = 0                           end = (1 + 2) / 2     control = 1
-- start = (1 + 2) / 2                 end = (2 + 3) / 2     control = 2
-- start = (2 + 3) / 2                 end = (3 + 4) / 2     control = 3
-- .                                   .                     .
-- .                                   .                     .
-- start = ((n - 3) + (n - 2)) / 2     end = n - 1           control = n - 2
pointsToQuadCurves :: List Point -> List QuadraticCurve
pointsToQuadCurves points =
  case Tuple (head points) (last points) of
    Tuple (Just headPoint) (Just lastPoint) ->
      let n = length points
          slice1 = slice 1 (n - 3) points
          slice2 = slice 2 (n - 2) points
          midPoints = zipWith middlePoint slice1 slice2

          startPoints = headPoint : midPoints
          endPoints = snoc midPoints lastPoint
          controlPoints = slice 1 (n - 2) points
       in map toQuadCurve $ zip (zip startPoints endPoints) controlPoints
    _ -> Nil
  where
    toQuadCurve :: Tuple (Tuple Point Point) Point -> QuadraticCurve
    toQuadCurve (Tuple (Tuple start end) control) =
      { start: start
      , end: end
      , control: control
      }
