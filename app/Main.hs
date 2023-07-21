module Main (main) where

import           AoCUtils.Days        (readInput)
import           AoCUtils.Interactive (aocMain)
import           Data.HashMap.Lazy    (HashMap)
import qualified Data.HashMap.Lazy    as HM
import           Day11.Graphics       (renderDay11part1, renderDay11part2)
import           Day13.Graphics       (renderDay13part1, renderDay13part2)
import           Day15.Graphics       (renderRobotSearch)
import           Utils.Days           (solvers)
import           Utils.Parsing        (parseICProgram)

main :: IO ()
main = aocMain solvers graphicalMap

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

day15GraphicalRobot :: IO ()
day15GraphicalRobot = do
  input <- readInput 15
  let program = parseICProgram input
  renderRobotSearch program

graphicalMap :: HashMap String (IO ())
graphicalMap = HM.fromList [
  ("13.1", day13Graphical1),
  ("13.2", day13Graphical2),
  ("11.1", day11Graphical1),
  ("11.2", day11Graphical2),
  ("15R", day15GraphicalRobot)]
