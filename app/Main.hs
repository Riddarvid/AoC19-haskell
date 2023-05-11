module Main (main) where
import           Day1                (solve)
import           Day10               (solve)
import           Day11               (solve)
import           Day12               (solve)
import           Day13               (solve)
import           Day2                (solve)
import           Day3                (solve)
import           Day4                (solve)
import           Day5                (solve)
import           Day6                (solve)
import           Day7                (solve)
import           Day8                (solve)
import           Day9                (solve)

import           Control.Applicative ((<**>), (<|>))
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Time           (diffUTCTime, getCurrentTime)
import           Options.Applicative (Parser, ParserInfo, argument, auto,
                                      execParser, fullDesc, header, help,
                                      helper, info, metavar, progDesc, short,
                                      strOption)
import           Utils.Graphics      (renderDay13Start)
import           Utils.Parsing       (parseICProgram)
import           Utils.Solution      (Solver, showSolution)

data ProgramOpts = Textual Int | Graphical String | TextualAll

main :: IO ()
main = do
  options <- execParser opts
  case options of
    Textual day     -> printSolutions [day]
    Graphical visId -> displayGraphical visId
    TextualAll      -> printSolutions [1 .. length solvers]

-- Opts parsing

opts :: ParserInfo ProgramOpts
opts = info (optsParser <**> helper)
  (fullDesc
  <> progDesc "Solve a day or show visualization"
  <> header "AoC Solver" )

optsParser :: Parser ProgramOpts
optsParser = textualParser <|> graphicalParser <|> allParser

textualParser :: Parser ProgramOpts
textualParser = Textual <$> argument auto (
  metavar "DAY"
  <> help "Day to run.")

graphicalParser :: Parser ProgramOpts
graphicalParser = Graphical <$> strOption
  (short 'g'
  <> metavar "VISUALIZATION"
  <> help "Visualization to run")

allParser :: Parser ProgramOpts
allParser = pure TextualAll

-- Textual interface

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

solvers :: [Solver]
solvers = [Day1.solve, Day2.solve, Day3.solve, Day4.solve, Day5.solve, Day6.solve, Day7.solve,
  Day8.solve, Day9.solve, Day10.solve, Day11.solve, Day12.solve, Day13.solve]

-- Visualizations

day13Graphical :: IO ()
day13Graphical = do
  input <- readInput 13
  let program = parseICProgram input
  renderDay13Start program

displayGraphical :: String -> IO ()
displayGraphical str = case Map.lookup str graphicalMap of
  Nothing  -> error $ "No visualization mapped to identifier: " ++ str
  Just vis -> vis

graphicalMap :: Map String (IO ())
graphicalMap = Map.fromList [("13.1", day13Graphical)]

-- Utils ----------------------------

dashLine :: String
dashLine = "---------------------------------"

readInput :: Int -> IO String
readInput day = readFile ("input/input" ++ show day ++ ".txt")
