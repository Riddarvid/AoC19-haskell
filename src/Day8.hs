module Day8 (solve) where
import           Data.Char      (digitToInt)
import           Data.Foldable  (minimumBy)
import           Data.Ord       (comparing)
import           GHC.Utils.Misc (chunkList)
import           Utils.Solution (Solver)

type Layer = [Int]

solve :: Solver
solve input = let
  stream = chunkList (25 * 6) $ map digitToInt $ head $ lines input
  part1 = solve1 stream
  part2 = solve2 stream
  in (show part1, part2)

---------------------

solve1 :: [Layer] -> Integer
solve1 layers = toInteger (count 1 minZerosLayer) * toInteger (count 2 minZerosLayer)
  where
    minZerosLayer = minimumBy (comparing (count 0)) layers

count :: (Eq a) => a -> [a] -> Int
count target = foldr (\x acc -> if x == target then acc + 1 else acc) 0

-------------------------

solve2 :: [Layer] -> String
solve2 = showImage . combineLayers

combineLayers :: [Layer] -> Layer
combineLayers = foldr1 (zipWith (\top bottom -> if top == 2 then bottom else top))

showImage :: Layer -> String
showImage = unlines . chunkList 25 . map (\c -> if c == 1 then '#' else ' ')
