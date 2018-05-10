module Geometry where

newtype Point a = Point (a, a) deriving (Show, Eq)
newtype Vec a   = Vec (a, a)   deriving (Show, Eq)

data Segment a = Segment (Point a) (Point a) deriving (Show, Eq)
data Circle  a = Circle  (Point a) a         deriving (Show, Eq)
data Square  a = Square  (Point a) a         deriving (Show, Eq)

instance Num a => Num (Point a) where
  Point (x, y) + Point (x', y') = Point (x + x', y + y')
  Point (x, y) - Point (x', y') = Point (x - x', y - y')
  Point (x, y) * Point (x', y') = Point (x * x', y * y')
  abs    (Point (a, b)) = Point (abs a, abs b)
  signum (Point (a, b)) = Point (signum a, signum b)
  fromInteger i = Point (fromInteger i, fromInteger i)

infixl 4 ~==
(~==) :: (Floating a, Ord a) => a -> a -> Bool
(~==) x y = abs (x - y) < (1e-6)

infixl 4 ~<=
(~<=) :: (Floating a, Ord a) => a -> a -> Bool
(~<=) x y = x ~== y || x < y

dot :: Num a => Vec a -> Vec a -> a
dot (Vec (x, y)) (Vec (x', y')) = x * x' + y * y'

distance :: Floating a => Point a -> Point a -> a
distance (Point (x, y)) (Point (x', y')) = sqrt ((x - x') ^ 2 + (y - y') ^ 2)

multiplyByScalar :: Num a => Vec a -> a -> Vec a
multiplyByScalar (Vec (x, y)) s = Vec (x * s, y * s)

vecFromPoint :: Point a -> Vec a
vecFromPoint (Point (x, y)) = Vec (x, y)

pointPlusVec :: Num a => Point a -> Vec a -> Point a
pointPlusVec (Point (px, py)) (Vec (vx, vy)) = Point (px + vx, py + vy)

circleIntersectSeg :: (Floating a, Ord a) => Circle a -> Segment a -> Bool
circleIntersectSeg (Circle center rad) seg = distance center (closestPointOnSeg center seg) ~<= rad

circlesIntersect :: (Floating a, Ord a) => Circle a -> Circle a -> Bool
circlesIntersect (Circle center rad) (Circle center' rad') = distance center center' ~<= rad + rad'

pointInCircle :: (Floating a, Ord a) => Point a -> Circle a -> Bool
pointInCircle point (Circle center radius) = distance point center ~<= radius

squareInsideCircle :: (Floating a, Ord a) => Square a -> Circle a -> Bool
squareInsideCircle square circle = all (\v -> pointInCircle v circle) (vertices square)

vertices :: Num a => Square a -> [Point a]
vertices square = [ topLeftVertex square
                  , topRightVertex square
                  , bottomLeftVertex square
                  , bottomRightVertex square
                  ]

topLeftVertex :: Num a => Square a -> Point a
topLeftVertex (Square point _) = point

topRightVertex :: Num a => Square a -> Point a
topRightVertex (Square point side) = point + Point (side, 0)

bottomLeftVertex :: Num a => Square a -> Point a
bottomLeftVertex (Square point side) = point + Point (0, side)

bottomRightVertex :: Num a => Square a -> Point a
bottomRightVertex (Square point side) = point + Point (side, side)

pointOnSeg :: (Floating a, Ord a) => Point a -> Segment a -> Bool
pointOnSeg c (Segment a b) = (distance a c + distance c b) ~== distance a b

pointToSegEnds :: Floating a => Point a -> Segment a -> a
pointToSegEnds p (Segment a b) = distance a p + distance b p

closestPointOnSeg :: (Floating a, Ord a) => Point a -> Segment a -> Point a
closestPointOnSeg p seg = case projectPointOnSeg p seg of
                            Just point -> point
                            Nothing    -> closestEndOnSeg p seg
  where
    closestEndOnSeg c (Segment a b) = if distance c a < distance c b
                                        then a
                                        else b

projectPointOnSeg :: (Floating a, Ord a) => Point a -> Segment a -> Maybe (Point a)
projectPointOnSeg c (Segment a b) = if pointOnSeg p (Segment a b)
                                      then Just p
                                      else Nothing
  where
    ab = vecFromPoint (b - a)
    ac = vecFromPoint (c - a)
    p  = pointPlusVec a (multiplyByScalar ab (dot ab ac / dot ab ab))
