module Day11 (
  solve,
  runRobot,
  RobotState(..)
) where

import           AoCUtils.Days       (Solver)
import           AoCUtils.Geometry   (Point (moveBy, origo), Point2, Vector2,
                                      turnLeft, turnRight, upV)
import           AoCUtils.Show       (showPoints)
import           Control.Monad.State (State, evalState, execState, get, gets,
                                      modify)
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import           Utils.Intcode       (IntcodeComputer, Program, isHalted,
                                      makeIC, runComputer, setInput)
import           Utils.Parsing       (parseICProgram)

data RobotState = RS {
  rPos         :: Point2 Integer,
  rHeading     :: Vector2 Integer,
  rEverPainted :: HashSet (Point2 Integer),
  rWhite       :: HashSet (Point2 Integer),
  rIntCode     :: IntcodeComputer
} deriving (Show)

solve :: Solver
solve input = let
  program = parseICProgram input
  part1 = solve1 program
  part2 = solve2 program
  in (show part1, part2)

-- Part 1

solve1 :: Program -> Int
solve1 program = HS.size $ rEverPainted endState
  where
    startState = makeStartState program False
    endState = execState paintCycle startState

-- Part 2

solve2 :: Program -> String
solve2 program = showPainted $ rWhite endState
  where
    startState = makeStartState program True
    endState = execState paintCycle startState

showPainted :: HashSet (Point2 Integer) -> String
showPainted = showPoints

-- Visualization

runRobot :: Program -> Bool -> [RobotState]
runRobot program startWhite = evalState paintCycle startState
  where
    startState = makeStartState program startWhite

-- General

makeStartState :: Program -> Bool -> RobotState
makeStartState program startOnWhite = RS {
  rPos = startPos,
  rHeading = upV,
  rEverPainted = painted,
  rWhite = whites,
  rIntCode = makeIC program []}
  where
    (painted, whites) = if startOnWhite
      then (HS.singleton startPos, HS.singleton startPos)
      else (HS.empty, HS.empty)
    startPos = origo

currentColor :: State RobotState Integer
currentColor = do
  pos <- gets rPos
  whites <- gets rWhite
  return $ if HS.member pos whites then 1 else 0

paint :: Integer -> State RobotState ()
paint color = do
  pos <- gets rPos
  everPainted <- gets rEverPainted
  whites <- gets rWhite
  let everPainted' = HS.insert pos everPainted
  let whites' = if color == 1 then HS.insert pos whites else HS.delete pos whites
  modify (\s -> s{rEverPainted = everPainted', rWhite = whites'})

turn :: Integer -> State RobotState ()
turn direction = do
  heading <- gets rHeading
  let heading' = if direction == 1 then turnRight heading else turnLeft heading
  modify (\s -> s{rHeading = heading'})

move :: State RobotState ()
move = do
  pos <- gets rPos
  heading <- gets rHeading
  let pos' = moveBy pos heading
  modify (\s -> s{rPos = pos'})

-- Process:
-- 1) If halted, done. If waiting, add current color to input.
-- 2) Run intcode program
-- 3) Read output, paint and move accordingly.
paintCycle :: State RobotState [RobotState]
paintCycle = do
  rs <- get
  ic <- gets rIntCode
  if isHalted ic
    then return [rs]
    else do
      color <- currentColor
      let input = [color]
      let ic' = setInput ic input
      let (output, ic'') = runComputer ic'
      modify (\s -> s{rIntCode = ic''})
      case output of
        [color', direction] -> do
          paint color'
          turn direction
          move
          rss <- paintCycle
          return (rs : rss)
        _                   -> error $ "Unrecognized output: " ++ show output
