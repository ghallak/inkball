module InkBall.Physics
  ( moveBall
  , fallInSink
  , collideWithBlock
  , collideWithBall
  , collideWithInkLine
  ) where

import Prelude

import Data.Foldable (findMap)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)

import InkBall.Constants (blockSide)
import InkBall.GameObjects
  (BlockSide(..), InkDot(..), Color(..), InkLine, Ball, Block, Sink)
import InkBall.Geometry
  (pointToSegEnds, circleIntersectSeg, circlesIntersect, circlesTouchingPoint,
  normalize, multiplyByScalar, dot)

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

-- https://gamedev.stackexchange.com/questions/112299/balls-velocity-vector-reflect-against-a-point
-- https://www.gamasutra.com/view/feature/131424/pool_hall_lessons_fast_accurate_.php?page=3
collideWithInkLine :: Ball -> InkLine -> Maybe Ball
collideWithInkLine ball ink = findMap (collideWithInkDot ball) ink

collideWithInkDot :: Ball -> InkDot -> Maybe Ball
collideWithInkDot ball (InkDot inkDot) =
  let touchPoint = circlesTouchingPoint ball.circle inkDot
      n = normalize $ touchPoint - inkDot.center
      v = ball.velocity - multiplyByScalar n (2.0 * dot ball.velocity n)
   in if circlesIntersect ball.circle inkDot
        then Just ball { velocity = v }
        else Nothing
