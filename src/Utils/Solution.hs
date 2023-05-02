module Utils.Solution (
  Solution,
  Solver,
  showSolution
) where

type Solution = (String, String)

type Solver = String -> Solution

showSolution :: Solution -> String
showSolution (part1, part2) = "Part1:\n" ++ part1 ++ "\n\n" ++ "Part2:\n" ++ part2
