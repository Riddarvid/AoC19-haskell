module Utils.Show (showPoints) where

import           Data.Hashable  (Hashable)
import           Data.HashSet   (HashSet)
import qualified Data.HashSet   as HS
import           Utils.Geometry (Point)

showPoints :: (Enum a, Hashable a, Ord a) => HashSet (Point a) -> String
showPoints points = unlines $ map (showPointRow points minX maxX) [minY .. maxY]
  where
    (minX, minY, maxX, maxY) = findDimensions points

showPointRow :: (Enum a, Hashable a) => HashSet (Point a) -> a -> a -> a -> String
showPointRow points minX maxX y = map (\x -> showPoint points x y) [minX .. maxX]

showPoint :: (Hashable a) => HashSet (Point a) -> a -> a -> Char
showPoint points x y
  | HS.member (x, y) points = '#'
  | otherwise = ' '

findDimensions :: (Ord a, Hashable a) => HashSet (Point a) -> (a, a, a, a)
findDimensions points = (minimum xs, minimum ys, maximum xs, maximum ys)
  where
    xs = HS.map fst points
    ys = HS.map snd points
