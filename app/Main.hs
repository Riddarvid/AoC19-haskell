module Main (main) where
import           Day1

import           Utils.Solution (Solution, Solver, showSolution)

main :: IO ()
main = do
  input <- readFile ("input/input" ++ show day ++ ".txt")
  let solution = (solvers !! (day - 1)) input
  putStrLn $ showSolution solution
  where
    day = 1

solvers :: [Solver]
solvers = [Day1.solve]
