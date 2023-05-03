module Main (main) where
import           Day1           (solve)
import           Day2           (solve)
import           Day3           (solve)
import           Day4           (solve)
import           Day5           (solve)
import           Day6           (solve)
import           Day7           (solve)
import           Day8           (solve)

import           Utils.Solution (Solver, showSolution)

main :: IO ()
main = do
  input <- readFile ("input/input" ++ show day ++ ".txt")
  let solution = (solvers !! (day - 1)) input
  putStrLn $ showSolution solution
  where
    day = 8

solvers :: [Solver]
solvers = [Day1.solve, Day2.solve, Day3.solve, Day4.solve, Day5.solve, Day6.solve, Day7.solve, Day8.solve]
