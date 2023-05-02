module Day1 (solve) where
import           Utils.Solution (Solver)

solve :: Solver
solve input = let
  modules = parse input
  part1 = solve1 modules
  part2 = solve2 modules
  in (show part1, show part2)

parse :: String -> [Integer]
parse = map read . lines

solve1 :: [Integer] -> Integer
solve1 = sum . map getFuel1

getFuel1 :: Integer -> Integer
getFuel1 mass = (mass `div` 3) - 2

solve2 :: [Integer] -> Integer
solve2 = sum . map getFuel2

getFuel2 :: Integer -> Integer
getFuel2 = sum . tail . takeWhile (> 0) . iterate getFuel1
