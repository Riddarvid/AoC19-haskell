module Main (main) where
import           Day1           (solve)
import           Day2           (solve)
import           Day3           (solve)

import           Utils.Solution (Solver, showSolution)

main :: IO ()
main = do
  input <- readFile ("input/input" ++ show day ++ ".txt")
  let solution = (solvers !! (day - 1)) input
  putStrLn $ showSolution solution
  where
    day = 3

solvers :: [Solver]
solvers = [Day1.solve, Day2.solve, Day3.solve]