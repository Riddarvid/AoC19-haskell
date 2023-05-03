module Day5 (solve) where
import           Utils.Intcode  (Program, execProgram, getOutput)
import           Utils.Parsing  (parseICProgram)
import           Utils.Solution (Solver)

solve :: Solver
solve input = let
  program = parseICProgram input
  part1 = solve1 program
  part2 = solve2 program
  in (show part1, show part2)

solve1 :: Program -> Integer
solve1 program = head $ runTest program [1]

solve2 :: Program -> [Integer]
solve2 program = runTest program [5]

runTest :: Program -> [Integer] -> [Integer]
runTest program input = getOutput $ execProgram program input
