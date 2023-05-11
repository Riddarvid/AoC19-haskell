module Day11.Graphics (
  renderDay11part1,
  renderDay11part2
) where
import qualified Data.HashSet   as HS
import           Day11          (RobotState (rPos, rWhite), runRobot)
import           Graphics.Gloss (Display (InWindow), Picture, animate, black,
                                 pictures, red, scale, white)
import           Utils.Graphics (renderSquare, tick)
import           Utils.Intcode  (Program)

day11Display :: Display
day11Display = InWindow "Day13" (900, 900) (10, 10)

renderDay11part1 :: Program -> IO ()
renderDay11part1 program = renderRobotProgram program False

renderDay11part2 :: Program -> IO ()
renderDay11part2 program = renderRobotProgram program True

-- General

renderRobotProgram :: Program -> Bool -> IO ()
renderRobotProgram program startWhite = animate day11Display black $ tick 0.01 pics
  where
    robotStates = runRobot program startWhite
    pics = map renderRS robotStates

renderRS :: RobotState -> Picture
renderRS rs = scale scale' scale' $ tilePics <> robotPic
  where
    robotPic = renderSquare red (rPos rs)
    tilePics = pictures $ map (renderSquare white) $ HS.toList (rWhite rs)
    scale' = 10
