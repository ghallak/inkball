module InkBall.Physics
  ( moveBall
  , fallInSink
  , neighborBlocks
  , collideWithBlock
  , collideWithBall
  , collideWithInkLine
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.HashMap as HM
import Data.Int (floor, round)
import Data.Foldable (findMap)
import Data.List (List(..), concatMap, fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Math (acos, pi)

import InkBall.Constants (blockSide, betweenCells)
import InkBall.GameObjects
  (BlockSide(..), InkDot(..), Color(..), InkLine, Ball, Block, Sink,
  BoardCoordinate, generateBlocksMap, mkInkDot)
import InkBall.Geometry
  (Vec, circleIntersectSeg, circlesIntersect, circlesTouchingPoint, normalize,
  pointToSegEnds, multiplyByScalar, dot, magnitude, pointsToQuadCurves, splitQuadCurve, quadCurveToSegments, circleIntersectSeg')

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

neighborBlocks :: Ball -> Array Block
neighborBlocks ball =
  let bc =
        { col: floor $ (ball.circle.center.x - betweenCells) / (blockSide + betweenCells)
        , row: floor $ (ball.circle.center.y - betweenCells) / (blockSide + betweenCells)
        }
      z = angle ball.velocity

      rr = blockAt { row: bc.row    , col: bc.col + 1 }
      rd = blockAt { row: bc.row + 1, col: bc.col + 1 }
      dd = blockAt { row: bc.row + 1, col: bc.col     }
      ld = blockAt { row: bc.row + 1, col: bc.col - 1 }
      ll = blockAt { row: bc.row    , col: bc.col - 1 }
      lu = blockAt { row: bc.row - 1, col: bc.col - 1 }
      uu = blockAt { row: bc.row - 1, col: bc.col     }
      ru = blockAt { row: bc.row - 1, col: bc.col + 1 }

      check = catMaybes $
        if      z >= 0.0            && z <= pi / 2.0       then [rr, rd, dd]
        else if z >= pi / 2.0       && z <= pi             then [dd, ld, ll]
        else if z >= pi             && z <= 3.0 * pi / 2.0 then [ll, lu, uu]
        else if z >= 3.0 * pi / 2.0 && z <= 2.0 * pi       then [uu, ru, rr]
        else []

   in check
  where
    blockAt :: BoardCoordinate -> Maybe Block
    blockAt coor = HM.lookup coor generateBlocksMap

    angle :: Vec -> Number
    angle v =
      let theta = acos $ dot v { x: 1.0, y: 0.0 } / magnitude v
          sign = if v.y > 0.0 then 1.0 else -1.0
          result = theta * sign
       in if result > 0.0 then result else 2.0 * pi + result

collideWithBlock :: Ball -> Block -> Maybe Ball
collideWithBlock ball block =
  case detectSide of
    Just side -> Just $ changeDirection (ball { color = newBallColor }) side
    Nothing   -> Nothing
  where
    detectSide :: Maybe BlockSide
    detectSide
      | topIntersect    && leftIntersect  = if pointToSegEnds center topSeg < pointToSegEnds center leftSeg
                                               then Just TopSide
                                               else Just LeftSide
      | topIntersect    && rightIntersect = if pointToSegEnds center topSeg < pointToSegEnds center rightSeg
                                               then Just TopSide
                                               else Just RightSide
      | bottomIntersect && leftIntersect  = if pointToSegEnds center bottomSeg < pointToSegEnds center leftSeg
                                               then Just BottomSide
                                               else Just LeftSide
      | bottomIntersect && rightIntersect = if pointToSegEnds center bottomSeg < pointToSegEnds center rightSeg
                                               then Just BottomSide
                                               else Just RightSide
      | topIntersect    = Just TopSide
      | bottomIntersect = Just BottomSide
      | leftIntersect   = Just LeftSide
      | rightIntersect  = Just RightSide
      | otherwise       = Nothing

    newBallColor = if block.color == White then ball.color else block.color
    center = ball.circle.center
    bx = block.square.topLeft.x
    by = block.square.topLeft.y
    topLeft     = { x: bx            , y: by }
    topRight    = { x: bx + blockSide, y: by }
    bottomLeft  = { x: bx            , y: by + blockSide }
    bottomRight = { x: bx + blockSide, y: by + blockSide }
    topSeg    = { p: topLeft   , q: topRight }
    leftSeg   = { p: topLeft   , q: bottomLeft }
    bottomSeg = { p: bottomLeft, q: bottomRight }
    rightSeg  = { p: topRight  , q: bottomRight }
    topIntersect    = circleIntersectSeg ball.circle topSeg
    bottomIntersect = circleIntersectSeg ball.circle bottomSeg
    leftIntersect   = circleIntersectSeg ball.circle leftSeg
    rightIntersect  = circleIntersectSeg ball.circle rightSeg

changeDirection :: Ball -> BlockSide -> Ball
changeDirection ball TopSide    = ball { velocity { y = negate $ abs ball.velocity.y } }
changeDirection ball BottomSide = ball { velocity { y = abs ball.velocity.y } }
changeDirection ball LeftSide   = ball { velocity { x = negate $ abs ball.velocity.x } }
changeDirection ball RightSide  = ball { velocity { x = abs ball.velocity.x } }

fallInSink :: Sink -> Ball -> Boolean
fallInSink sink ball = circlesIntersect sink.circle ball.circle

collideWithBall :: Ball -> Ball -> Maybe Ball
collideWithBall ball ball' =
  let ballsCollide = circlesIntersect ball.circle ball'.circle
   in if ball.circle /= ball'.circle && ballsCollide
        then Just $ ball { velocity = ball'.velocity }
        else Nothing

-- 1. convert inkline to a list of quad curves
-- 2. split each one of the resulted quad curves into 4
-- 3. convert each quad curves into list of segments
-- 4. check collision against segments
collideWithInkLine :: Ball -> InkLine -> Maybe Ball
collideWithInkLine ball inkLine =
  let curves = pointsToQuadCurves $ map (\(InkDot circle) -> circle.center) (fromFoldable inkLine)
      curvesX4 = concatMap ((\(Tuple curve curve') -> (f $ splitQuadCurve curve) <> (f $ splitQuadCurve curve')) <<< splitQuadCurve) curves
      segments = concatMap (f <<< quadCurveToSegments) curvesX4
      intersectionPoint = findMap (circleIntersectSeg' ball.circle) segments
   in case intersectionPoint of
        Just point -> collideWithInkDot ball (mkInkDot { x: round point.x, y: round point.y })
        Nothing -> Nothing
  where
    f :: forall a. Tuple a a -> List a
    f (Tuple x y) = x : y : Nil

-- https://gamedev.stackexchange.com/questions/112299/balls-velocity-vector-reflect-against-a-point
-- https://www.gamasutra.com/view/feature/131424/pool_hall_lessons_fast_accurate_.php?page=3
collideWithInkDot :: Ball -> InkDot -> Maybe Ball
collideWithInkDot ball (InkDot inkDot) =
  let touchPoint = circlesTouchingPoint ball.circle inkDot
      n = normalize $ touchPoint - inkDot.center
      v = ball.velocity - multiplyByScalar n (2.0 * dot ball.velocity n)
   in if circlesIntersect ball.circle inkDot
        then Just ball { velocity = v }
        else Nothing
