module Main (main) where


import           Control.Applicative ((<**>), (<|>))
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Time           (diffUTCTime, getCurrentTime)
import           Day11.Graphics      (renderDay11part1, renderDay11part2)
import           Day13.Graphics      (renderDay13part1, renderDay13part2)
import           Options.Applicative (Parser, ParserInfo, argument, auto,
                                      execParser, fullDesc, header, help,
                                      helper, info, metavar, progDesc, short,
                                      strOption)
import           Utils.Days          (readInput, solvers)
import           Utils.Parsing       (parseICProgram)
import           Utils.Solution      (showSolution)

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

-- Visualizations

day11Graphical1 :: IO ()
day11Graphical1 = do
  input <- readInput 11
  let program = parseICProgram input
  renderDay11part1 program

day11Graphical2 :: IO ()
day11Graphical2 = do
  input <- readInput 11
  let program = parseICProgram input
  renderDay11part2 program

day13Graphical1 :: IO ()
day13Graphical1 = do
  input <- readInput 13
  let program = parseICProgram input
  renderDay13part1 program

day13Graphical2 :: IO ()
day13Graphical2 = do
  input <- readInput 13
  let program = parseICProgram input
  renderDay13part2 program

displayGraphical :: String -> IO ()
displayGraphical str = case Map.lookup str graphicalMap of
  Nothing  -> error $ "No visualization mapped to identifier: " ++ str
  Just vis -> vis

graphicalMap :: Map String (IO ())
graphicalMap = Map.fromList [
  ("13.1", day13Graphical1),
  ("13.2", day13Graphical2),
  ("11.1", day11Graphical1),
  ("11.2", day11Graphical2)]

-- Utils ----------------------------

dashLine :: String
dashLine = "---------------------------------"
