module Utils.Geometry (
  Point,
  Vector,
  origo,
  distanceBetween,
  distanceFromOrigo,
  move,
  upV,
  rightV,
  downV,
  leftV,
  scaleBy
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

move :: (Num a) => Point a -> Vector a -> Point a
move (x, y) (dx, dy) = (x + dx, y + dy)

scaleBy :: Num a => Vector a -> a -> Vector a
scaleBy (dx, dy) s = (s * dx, s * dy)

upV :: Num a => Vector a
upV = (0, -1)

rightV :: Num a => Vector a
rightV = (1, 0)

downV :: Num a => Vector a
downV = (0, 1)

leftV :: Num a => Vector a
leftV = (-1, 0)
