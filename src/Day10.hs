module Day10 (solve, laserAngle, partitionVisible, groupBy, sameAngle, group) where
import           Utils.Solution    (Solver)

import           AoCUtils.Geometry (Point (distanceBetween, vectorBetween),
                                    Point2 (P2), Vector2)
import qualified Data.HashSet      as HS
import           Data.List         (delete, group, groupBy, maximumBy, sortOn)
import           Data.Maybe        (catMaybes)
import           Data.Ord          (comparing)

solve :: Solver
solve input = let
  asteroids = parseAsteroids input
  (best, part1) = solve1 asteroids
  part2 = solve2 asteroids best
  in (show part1, show part2)

type Asteroid = Point2 Integer

parseAsteroids :: String -> [Asteroid]
parseAsteroids = concat . zipWith parseAsteroidRow [0..] . lines

parseAsteroidRow :: Integer -> String -> [Asteroid]
parseAsteroidRow y row = catMaybes $ zipWith (\x c -> if c == '#' then Just (P2 x y) else Nothing) [0..] row

---------------------

solve1 :: [Asteroid] -> (Asteroid, Int)
solve1 asteroids = (best, nVisible asteroids best)
  where
    best = maximumBy (comparing $ nVisible asteroids) asteroids

nVisible :: [Asteroid] -> Asteroid -> Int
nVisible asteroids target = HS.size $ foldr (HS.insert . angle) HS.empty asteroids'
  where
    asteroids' = delete target asteroids
    angle :: Asteroid -> Double
    angle asteroid = let (P2 x y) = vectorBetween target asteroid in atan2 (fromIntegral y) (fromIntegral x)

sameAngle :: (Integral a) => Vector2 a -> Vector2 a -> Bool
sameAngle v1 v2 = laserAngle v1 == laserAngle v2

laserAngle :: Integral a => Vector2 a -> Double
laserAngle (P2 x y) = angle''
  where
    angle = atan2 (fromIntegral y) (fromIntegral x)
    angle' = angle + (pi / 2)
    angle'' = if angle' < 0 then angle' + 2 * pi else angle'


solve2 :: [Asteroid] -> Asteroid -> Integer
solve2 asteroids station = x * 100 + y
  where
    asteroids' = delete station asteroids
    (P2 x y) = laserSort station asteroids' !! 199

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
