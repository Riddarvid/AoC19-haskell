module Utils.Days (
  solvers,
  readInput
) where
import           Day1           (solve)
import           Day10          (solve)
import           Day11          (solve)
import           Day12          (solve)
import           Day13          (solve)
import           Day14          (solve)
import           Day2           (solve)
import           Day3           (solve)
import           Day4           (solve)
import           Day5           (solve)
import           Day6           (solve)
import           Day7           (solve)
import           Day8           (solve)
import           Day9           (solve)
import           Utils.Solution (Solver)

solvers :: [Solver]
solvers = [Day1.solve, Day2.solve, Day3.solve, Day4.solve, Day5.solve, Day6.solve, Day7.solve,
  Day8.solve, Day9.solve, Day10.solve, Day11.solve, Day12.solve, Day13.solve, Day14.solve]

readInput :: Int -> IO String
readInput day = readFile ("input/input" ++ show day ++ ".txt")
