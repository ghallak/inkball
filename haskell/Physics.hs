module Physics
       ( collide
       , afterCollide
       , moveBall
       ) where

import Data.List (find)

import GameObjects (Ball (..), Block (..), Velocity (..), InkDot (..), BlockSide (..), Color (Black), Sink, blockSide, getCircle, mkBall)
import Geometry (Square (..), Point (..), Circle (..), Segment (..), Vec (..), pointToSegEnds, circleIntersectSeg, circlesIntersect, dot, multiplyByScalar, normalize, vecFromPoint)

class Collide a where
  collide :: Ball -> a -> Bool
  afterCollide :: Ball -> a -> Ball

instance Collide a => Collide [a] where
  collide ball [] = False
  collide ball (x : xs) = collide ball x || collide ball xs

  afterCollide ball [] = ball
  afterCollide ball (x : xs)
    | collide ball x = afterCollide ball x
    | otherwise      = afterCollide ball xs

instance Collide Block where
  collide ball block = afterCollide ball block /= ball

  -- TODO: Handel color changing
  afterCollide ball block = case detectSide ball block of
                              Just s -> changeDirection ball s
                              Nothing -> ball
    where
      detectSide :: Ball -> Block -> Maybe BlockSide
      detectSide (Ball circle _ _) (Block (Square (Point (bx, by)) _) _)
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
        where
          center = getCenter circle
          getCenter (Circle center _) = center
          topLeft     = Point (bx, by)
          topRight    = Point (bx + blockSide, by)
          bottomLeft  = Point (bx, by + blockSide)
          bottomRight = Point (bx + blockSide, by + blockSide)
          topSeg    = Segment topLeft topRight
          leftSeg   = Segment topLeft bottomLeft
          bottomSeg = Segment bottomLeft bottomRight
          rightSeg  = Segment topRight bottomRight
          topIntersect    = circleIntersectSeg circle topSeg
          bottomIntersect = circleIntersectSeg circle bottomSeg
          leftIntersect   = circleIntersectSeg circle leftSeg
          rightIntersect  = circleIntersectSeg circle rightSeg

instance Collide Ball where
  collide ball ball' = ball /= ball' && circlesIntersect (getCircle ball) (getCircle ball')

  afterCollide (Ball circle _ color) (Ball _ v _) = Ball circle v color

instance Collide Sink where
  collide ball sink = circlesIntersect (getCircle ball) (getCircle sink)

  afterCollide _ _ = mkBall (0, 0) (Velocity (0, 0)) Black

-- TODO: Handle the case when ink is placed inside a ball
instance Collide InkDot where
  collide ball ink = circlesIntersect (getCircle ball) (getCircle ink)

  -- https://gamedev.stackexchange.com/questions/112299/balls-velocity-vector-reflect-against-a-point
  -- https://www.gamasutra.com/view/feature/131424/pool_hall_lessons_fast_accurate_.php?page=3
  afterCollide (Ball ballCircle v color) (InkDot inkCircle) = Ball ballCircle v' color
    where
      v' = toVel $ (toVec v) `minus` multiplyByScalar n (2 * (dot (toVec v) n))
      n  = normalize $ vecFromPoint (circlesTouchingPoint ballCircle inkCircle - circleCenter inkCircle)
      -- TODO: Move this function to Geometry module
      circlesTouchingPoint (Circle center radius) (Circle center' radius') = center + ((center' - center) * Point (radius / (radius + radius'), radius / (radius + radius')))
      circleCenter (Circle center _) = center
      toVec (Velocity (x, y)) = Vec (x, y)
      toVel (Vec (x, y)) = Velocity (x, y)
      minus (Vec (x, y)) (Vec (x', y')) = Vec (x - x', y - y')

changeDirection :: Ball -> BlockSide -> Ball
changeDirection (Ball circle (Velocity (dx, dy)) color) TopSide    = Ball circle (Velocity (dx, negate $ abs dy)) color
changeDirection (Ball circle (Velocity (dx, dy)) color) BottomSide = Ball circle (Velocity (dx, abs dy)) color
changeDirection (Ball circle (Velocity (dx, dy)) color) LeftSide   = Ball circle (Velocity (negate $ abs dx, dy)) color
changeDirection (Ball circle (Velocity (dx, dy)) color) RightSide  = Ball circle (Velocity (abs dx, dy)) color

moveBall :: Ball -> Ball
moveBall (Ball (Circle (Point (cx, cy)) _) (Velocity (vx, vy)) color) = mkBall (cx + vx, cy + vy) (Velocity (vx, vy)) color
