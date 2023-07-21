module Day9 (solve) where
import           AoCUtils.Days (Solver)
import           Utils.Intcode (Program, evalProgram)
import           Utils.Parsing (parseICProgram)

solve :: Solver
solve input = let
  program = parseICProgram input
  part1 = solve1 program
  part2 = solve2 program
  in (show part1, show part2)

solve1 :: Program -> Integer
solve1 program = head $ evalProgram program [1]

solve2 :: Program -> Integer
solve2 program = head $ evalProgram program [2]
