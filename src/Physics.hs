module Physics where

import GameObjects
import Geometry

-- TODO: Handel color changing
afterCollide :: Ball -> Block -> Ball
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

collide :: Ball -> Block -> Bool
collide ball block = afterCollide ball block /= ball

changeDirection :: Ball -> BlockSide -> Ball
changeDirection (Ball cen (Velocity (dx, dy)) color) TopSide    = Ball cen (Velocity (dx, negate $ abs dy)) color
changeDirection (Ball cen (Velocity (dx, dy)) color) BottomSide = Ball cen (Velocity (dx, abs dy)) color
changeDirection (Ball cen (Velocity (dx, dy)) color) LeftSide   = Ball cen (Velocity (negate $ abs dx, dy)) color
changeDirection (Ball cen (Velocity (dx, dy)) color) RightSide  = Ball cen (Velocity (abs dx, dy)) color

moveBall :: Ball -> Ball
moveBall (Ball (Center (cx, cy)) (Velocity (vx, vy)) color) = Ball (Center (cx + vx, cy + vy)) (Velocity (vx, vy)) color
