module Day10 (solve, laserAngle, partitionVisible, groupBy, sameAngle, group) where
import           Utils.Solution (Solver)

import           Data.HashSet   (HashSet)
import qualified Data.HashSet   as HS
import           Data.List      (delete, group, groupBy, maximumBy, sortOn)
import           Data.Maybe     (catMaybes)
import           Data.Ord       (comparing)
import           Utils.Geometry (Point, Vector, distanceBetween, vectorBetween)

solve :: Solver
solve input = let
  asteroids = parseAsteroids input
  (best, part1) = solve1 asteroids
  part2 = solve2 asteroids best
  in (show part1, show part2)

type Asteroid = Point Integer

parseAsteroids :: String -> [Asteroid]
parseAsteroids = concat . zipWith parseAsteroidRow [0..] . lines

parseAsteroidRow :: Integer -> String -> [Asteroid]
parseAsteroidRow y row = catMaybes $ zipWith (\x c -> if c == '#' then Just (x, y) else Nothing) [0..] row

---------------------

solve1 :: [Asteroid] -> (Asteroid, Int)
solve1 asteroids = (best, nVisible asteroids best)
  where
    best = maximumBy (comparing $ nVisible asteroids) asteroids

nVisible :: [Asteroid] -> Asteroid -> Int
nVisible asteroids target = length visible
  where
    asteroids' = delete target asteroids
    (visible, _) = partitionVisible target asteroids'

sameAngle :: (Integral a) => Vector a -> Vector a -> Bool
sameAngle v1 v2 = laserAngle v1 == laserAngle v2

laserAngle :: Integral a => Vector a -> Double
laserAngle (x, y) = angle'''''
  where
    angle = atan2 (fromIntegral y) (fromIntegral x)
    angle' = angle + pi -- angle' is between 0 and 2pi. 0 represents an angle pointing to the right.
    angle''' = angle'--2 * pi - angle' -- Angle is now measured clockwise
    angle'''' = angle''' - (pi / 2)
    angle''''' = if angle'''' < 0 then angle'''' + 2 * pi else angle''''


solve2 :: [Asteroid] -> Asteroid -> Integer
solve2 asteroids station = x * 100 + y
  where
    asteroids' = delete station asteroids
    (x, y) = laserSort station asteroids' !! 199

laserSort :: Asteroid -> [Asteroid] -> [Asteroid]
laserSort _ [] = []
laserSort laser asteroids = sortOn (laserAngle . vectorBetween laser) visible ++ laserSort laser nonVisible
  where
    (visible, nonVisible) = partitionVisible laser asteroids

partitionVisible :: Asteroid -> [Asteroid] -> ([Asteroid], [Asteroid])
partitionVisible laser asteroids = (concatMap (take 1) asteroids'', concatMap (drop 1) asteroids'')
  where
    asteroids' = groupBy (\a b -> sameAngle (vectorBetween laser a) (vectorBetween laser b)) $ sortOn (laserAngle . vectorBetween laser) asteroids
    asteroids'' = map (sortOn $ distanceBetween laser) asteroids'
