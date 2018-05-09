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
      detectSide (Ball (Center (cx, cy)) _ _) (Block (TopLeft (bx, by)) _)
        | topIntersect    && leftIntersect  = if pointToSegEnds (Point (cx, cy)) topSeg < pointToSegEnds (Point (cx, cy)) leftSeg
                                                 then Just TopSide
                                                 else Just LeftSide
        | topIntersect    && rightIntersect = if pointToSegEnds (Point (cx, cy)) topSeg < pointToSegEnds (Point (cx, cy)) rightSeg
                                                 then Just TopSide
                                                 else Just RightSide
        | bottomIntersect && leftIntersect  = if pointToSegEnds (Point (cx, cy)) bottomSeg < pointToSegEnds (Point (cx, cy)) leftSeg
                                                 then Just BottomSide
                                                 else Just LeftSide
        | bottomIntersect && rightIntersect = if pointToSegEnds (Point (cx, cy)) bottomSeg < pointToSegEnds (Point (cx, cy)) rightSeg
                                                 then Just BottomSide
                                                 else Just RightSide
        | topIntersect    = Just TopSide
        | bottomIntersect = Just BottomSide
        | leftIntersect   = Just LeftSide
        | rightIntersect  = Just RightSide
        | otherwise       = Nothing
        where
          circle = Circle (toPoint $ Center (cx, cy)) ballRadius
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
  collide ball ball' = ball /= ball' && circlesIntersect (toCircle ball) (toCircle ball')

  afterCollide (Ball center _ color) (Ball _ v _) = Ball center v color

changeDirection :: Ball -> BlockSide -> Ball
changeDirection (Ball cen (Velocity (dx, dy)) color) TopSide    = Ball cen (Velocity (dx, negate $ abs dy)) color
changeDirection (Ball cen (Velocity (dx, dy)) color) BottomSide = Ball cen (Velocity (dx, abs dy)) color
changeDirection (Ball cen (Velocity (dx, dy)) color) LeftSide   = Ball cen (Velocity (negate $ abs dx, dy)) color
changeDirection (Ball cen (Velocity (dx, dy)) color) RightSide  = Ball cen (Velocity (abs dx, dy)) color

moveBall :: Ball -> Ball
moveBall (Ball (Center (cx, cy)) (Velocity (vx, vy)) color) = Ball (Center (cx + vx, cy + vy)) (Velocity (vx, vy)) color
