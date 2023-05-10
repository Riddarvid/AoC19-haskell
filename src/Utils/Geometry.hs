module Utils.Geometry (
  Point,
  Vector,
  origo,
  distanceBetween,
  distanceFromOrigo,
  moveBy,
  upV,
  rightV,
  downV,
  leftV,
  scaleBy,
  vectorBetween,
  vectorLength,
  normalize,
  fromIntegralV,
  turnLeft,
  turnRight
) where

type Point a = (a, a)

type Vector a = (a, a)

origo :: (Num a) => Point a
origo = (0, 0)

-- Manhattan distance
distanceBetween :: (Num a) => Point a -> Point a -> a
distanceBetween (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

distanceFromOrigo :: (Num a) => Point a -> a
distanceFromOrigo = distanceBetween origo

moveBy :: (Num a) => Point a -> Vector a -> Point a
moveBy (x, y) (dx, dy) = (x + dx, y + dy)

scaleBy :: Num a => Vector a -> a -> Vector a
scaleBy (dx, dy) s = (s * dx, s * dy)

vectorBetween :: Num a => Point a -> Point a -> Vector a
vectorBetween (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

upV :: Num a => Vector a
upV = (0, -1)

rightV :: Num a => Vector a
rightV = (1, 0)

downV :: Num a => Vector a
downV = (0, 1)

leftV :: Num a => Vector a
leftV = (-1, 0)

vectorLength :: Floating a => Vector a -> a
vectorLength (x, y) = let
  in sqrt (x**2 + y**2)

normalize :: Floating a => Vector a -> Vector a
normalize v = scaleBy v (1 / vectorLength v)

fromIntegralV :: (Integral a, Num b) => Vector a -> Vector b
fromIntegralV (x, y) = (fromIntegral x, fromIntegral y)

-- Rotates a vector 90 degrees to the left
turnLeft :: Num a => Vector a -> Vector a
turnLeft (x, y) = (y, -x)

turnRight :: Num a => Vector a -> Vector a
turnRight (x, y) = (-y, x)
