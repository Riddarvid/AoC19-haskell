{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Day15 (
  solve,
  exploreMapPath,
  OxygenMap,
  Tile(..),
  Pos,
  RobotMove(..),
  Action(..)
) where
import           AoCUtils.Days        (Solver)
import           AoCUtils.Geometry    (Point (moveBy, origo), Point2 (P2),
                                       Vector2, downV, leftV, rightV, upV)
import           AoCUtils.Graphs      (BfsState (bfsNLayers),
                                       Goal (GFull, GTarget), bfsExplore,
                                       bfsPath)
import           Control.Monad.Except (Except, MonadError (throwError),
                                       runExcept, unless)
import           Control.Monad.Reader (MonadReader (ask, local),
                                       ReaderT (runReaderT))
import           Control.Monad.State  (MonadState, StateT (runStateT), gets,
                                       modify)
import           Data.Foldable        (find)
import           Data.HashMap.Lazy    (HashMap)
import qualified Data.HashMap.Lazy    as HM
import           Utils.Intcode        (IntcodeComputer, Program, makeIC,
                                       runComputer, setInput)
import           Utils.Parsing        (parseICProgram)

solve :: Solver
solve input = let
  program = parseICProgram input
  oxygenMap = exploreMap program
  adjacency = oxygenAdjacency oxygenMap
  source = findSource oxygenMap
  part1 = solve1 adjacency source
  part2 = solve2 adjacency source
  in (show part1, show part2)

-- Types

type Pos = Point2 Integer
data Direction = North | East | South | West

data Tile = Wall | Floor | Source
  deriving Show

type OxygenMap = HashMap Pos Tile

data OxygenState = OS {
  osOxygenMap :: OxygenMap,
  osComputer  :: IntcodeComputer,
  osPath      :: [RobotMove]
}

mkOxygenState :: Program -> OxygenState
mkOxygenState prg = OS {
  osOxygenMap = HM.empty,
  osComputer = makeIC prg [],
  osPath = [RobotMove origo Move]}

newtype RobotMonad a = RM (ReaderT Pos (StateT OxygenState (Except String)) a)
  deriving (Functor, Applicative, Monad, MonadState OxygenState, MonadReader Pos, MonadError String)

runRobotMonad :: RobotMonad a -> Pos -> OxygenState -> (a, OxygenState)
runRobotMonad (RM m) pos oxygenState = case result of
  Left msg   -> error msg
  Right res' -> res'
  where
    result = runExcept (runStateT (runReaderT m pos) oxygenState)

-- General

exploreMapPath :: Program -> (OxygenMap, [RobotMove])
exploreMapPath prg = (osOxygenMap st, osPath st)
  where
    (_, st) = runRobotMonad explore (P2 0 0) (mkOxygenState prg)

exploreMap :: Program -> OxygenMap
exploreMap prg = osOxygenMap oxygenState
  where
    (_, oxygenState) = runRobotMonad explore (P2 0 0) (mkOxygenState prg)

explore :: RobotMonad ()
explore = do
  exploreDir North
  exploreDir East
  exploreDir South
  exploreDir West

exploreDir :: Direction -> RobotMonad ()
exploreDir dir = do
  pos <- ask
  oxygenMap <- gets osOxygenMap
  let pos' = moveDir pos dir
  unless (HM.member pos' oxygenMap) $ do
    status <- tryMoveRobot dir
    let tile = statusToTile status
    modify (\s -> s{osOxygenMap = HM.insert pos' tile (osOxygenMap s)})
    if isOpen tile then do
      logPath pos' Move
      local (const pos') explore
      _ <- tryMoveRobot (oppositeDir dir)
      logPath pos Move
    else logPath pos' Explore

oppositeDir :: Direction -> Direction
oppositeDir North = South
oppositeDir South = North
oppositeDir East  = West
oppositeDir West  = East

moveDir :: Pos -> Direction -> Pos
moveDir pos dir = moveBy pos (dirToVec dir)

dirToVec :: Direction -> Vector2 Integer
dirToVec dir = case dir of
  North -> upV
  East  -> rightV
  South -> downV
  West  -> leftV

isOpen :: Tile -> Bool
isOpen Wall = False
isOpen _    = True

isSource :: Tile -> Bool
isSource Source = True
isSource _      = False

tryMoveRobot :: Direction -> RobotMonad Integer
tryMoveRobot dir = do
  comp <- gets osComputer
  let input = [dirCode dir]
  let comp' = setInput comp input
  let (output, comp'') = runComputer comp'
  case output of
    [status] -> do
      modify (\s -> s{osComputer = comp''})
      return status
    _        -> throwError "Status should be a single number."

dirCode :: Direction -> Integer
dirCode North = 1
dirCode South = 2
dirCode West  = 3
dirCode East  = 4

statusToTile :: Integer -> Tile
statusToTile 0 = Wall
statusToTile 1 = Floor
statusToTile 2 = Source
statusToTile _ = error "Invalid status"

data RobotMove = RobotMove Pos Action
data Action = Move | Explore

logPath :: Pos -> Action -> RobotMonad ()
logPath pos action = modify (\s -> s{osPath = RobotMove pos action : osPath s})

-- Part 1

type OxygenAdjacency = Pos -> [Pos]

solve1 :: OxygenAdjacency -> Pos -> Int
solve1 adjacency source = case bfsPath start target adjacency of
  Nothing   -> error "Path not found"
  Just path -> length path - 1
  where
    start = P2 0 0
    target = GTarget source

oxygenAdjacency :: OxygenMap -> Pos -> [Pos]
oxygenAdjacency oxygenMap pos = let neighbors = map (moveDir pos) [North, East, South, West]
  in filter (isFree oxygenMap) neighbors

isFree :: OxygenMap -> Pos -> Bool
isFree oxygenMap pos = maybe False isOpen (HM.lookup pos oxygenMap)

findSource :: OxygenMap -> Pos
findSource oxygenMap = case find (\(_, tile) -> isSource tile) (HM.toList oxygenMap) of
  Nothing       -> error "No source found"
  Just (pos, _) -> pos

-- Part 2

solve2 :: OxygenAdjacency -> Pos -> Integer
solve2 adjacency source = case bfsExplore source GFull adjacency of
  Nothing -> error "Search failed"
  Just st -> bfsNLayers st - 1
