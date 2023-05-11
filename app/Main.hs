module Main (main) where
import           Day1               (solve)
import           Day10              (solve)
import           Day11              (solve)
import           Day12              (solve)
import           Day13              (solve)
import           Day2               (solve)
import           Day3               (solve)
import           Day4               (solve)
import           Day5               (solve)
import           Day6               (solve)
import           Day7               (solve)
import           Day8               (solve)
import           Day9               (solve)

import           Data.Time          (diffUTCTime, getCurrentTime)
import           System.Environment (getArgs)
import           Utils.Graphics     (renderDay13Start)
import           Utils.Parsing      (parseICProgram)
import           Utils.Solution     (Solver, showSolution)

main :: IO ()
main = day13Graphical

day13Graphical :: IO ()
day13Graphical = do
  input <- readInput 13
  let program = parseICProgram input
  renderDay13Start program

printDays :: IO ()
printDays = do
  args <- getArgs
  case args of
    []       -> printSolutions [1 .. length solvers]
    [dayStr] -> printSolution $ read dayStr
    _        -> putStrLn "Unrecognized input, try again."

dashLine :: String
dashLine = "---------------------------------"

printSolutions :: [Int] -> IO ()
printSolutions days = do
  startTime <- getCurrentTime
  mapM_ printSolution days
  stopTime <- getCurrentTime
  putStrLn ""
  putStrLn dashLine
  putStrLn $ "\nAll puzzles solved in " ++ show (diffUTCTime stopTime startTime)

printSolution :: Int -> IO ()
printSolution day = do
  startTime <- getCurrentTime
  putStrLn dashLine
  putStrLn $ "Day " ++ show day
  input <- readInput day
  let solution = (solvers !! (day - 1)) input
  putStrLn $ showSolution solution
  stopTime <- getCurrentTime
  putStrLn $ "\nSolved in " ++ show (diffUTCTime stopTime startTime)

readInput :: Int -> IO String
readInput day = readFile ("input/input" ++ show day ++ ".txt")

solvers :: [Solver]
solvers = [Day1.solve, Day2.solve, Day3.solve, Day4.solve, Day5.solve, Day6.solve, Day7.solve,
  Day8.solve, Day9.solve, Day10.solve, Day11.solve, Day12.solve, Day13.solve]
