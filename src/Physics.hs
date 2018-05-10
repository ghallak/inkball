module Physics where

import GameObjects
import Geometry

class Collide a where
  collide :: Ball -> a -> Bool
  afterCollide :: Ball -> a -> Ball

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
    where
      getCircle (Ball circle _ _) = circle

  afterCollide (Ball circle _ color) (Ball _ v _) = Ball circle v color

changeDirection :: Ball -> BlockSide -> Ball
changeDirection (Ball circle (Velocity (dx, dy)) color) TopSide    = Ball circle (Velocity (dx, negate $ abs dy)) color
changeDirection (Ball circle (Velocity (dx, dy)) color) BottomSide = Ball circle (Velocity (dx, abs dy)) color
changeDirection (Ball circle (Velocity (dx, dy)) color) LeftSide   = Ball circle (Velocity (negate $ abs dx, dy)) color
changeDirection (Ball circle (Velocity (dx, dy)) color) RightSide  = Ball circle (Velocity (abs dx, dy)) color

moveBall :: Ball -> Ball
moveBall (Ball (Circle (Point (cx, cy)) _) (Velocity (vx, vy)) color) = mkBall (cx + vx, cy + vy) (Velocity (vx, vy)) color
