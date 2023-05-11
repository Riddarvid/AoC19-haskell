module Day13 (
  solve,
  GameState(..),
  Tile(..),
  getStartTiles,
  runGame
) where
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           GHC.Utils.Misc      (chunkList)
import           Utils.Geometry      (Point (origo), Point2 (P2))
import           Utils.Intcode       (IntcodeComputer, Program, evalProgram,
                                      isHalted, makeIC, runComputer, setInput)
import           Utils.Parsing       (parseICProgram)
import           Utils.Solution      (Solver)

data Tile = Empty | Wall | Block | Paddle | Ball
  deriving (Eq, Show)

data GameState = GS {
  gsTiles     :: HashMap (Point2 Integer) Tile,
  gsBallPos   :: Point2 Integer,
  gsPaddlePos :: Point2 Integer,
  gsScore     :: Integer
} deriving (Show)

emptyGS :: GameState
emptyGS = GS {gsTiles = HM.empty, gsBallPos = origo, gsPaddlePos = origo, gsScore = 0}

solve :: Solver
solve input = let
  program = parseICProgram input
  part1 = solve1 program
  part2 = solve2 program
  in (show part1, show part2)

-- Part 1

solve1 :: Program -> Int
solve1 = HM.size . HM.filter (== Block) . gsTiles . getStartTiles

getStartTiles :: Program -> GameState
getStartTiles program = interpretOutput emptyGS $ evalProgram program []

-- Part 2

solve2 :: Program -> Integer
solve2 = gsScore . last . runGame

runGame :: Program -> [GameState]
runGame program = map snd states'
  where
    program' = case program of
      []       -> error "Empty program"
      (_ : xs) -> 2 : xs
    startIC = makeIC program' []
    states = iterate (uncurry stepGame) (startIC, emptyGS)
    (running, halted) = span (\(ic, _) -> not $ isHalted ic) states
    states' = running ++ [head halted]

-- 1. Run computer
-- 2. Collect output into game state
-- 3. Provide appropriate input
stepGame :: IntcodeComputer -> GameState -> (IntcodeComputer, GameState)
stepGame ic gs = (ic'', gs')
  where
    (output, ic') = runComputer ic
    gs' = interpretOutput gs output
    joysticDir = determineDir gs'
    ic'' = setInput ic' [joysticDir]

determineDir :: GameState -> Integer
determineDir gs = case compare paddleX ballX of
  LT -> 1
  EQ -> 0
  GT -> (-1)
  where
    (P2 ballX _) = gsBallPos gs
    (P2 paddleX _) = gsPaddlePos gs

-- General

interpretOutput :: GameState -> [Integer] -> GameState
interpretOutput gs = foldl appendGS gs . chunkList 3

appendGS :: GameState -> [Integer] -> GameState
appendGS gs output = case interpretOutput' output of
  Left (pos, tile) -> gs{
    gsTiles = HM.insert pos tile (gsTiles gs),
    gsBallPos = ballPos',
    gsPaddlePos = paddlePos'}
    where
      ballPos' = if tile == Ball then pos else gsBallPos gs
      paddlePos' = if tile == Paddle then pos else gsPaddlePos gs
  Right score'     -> gs{gsScore = score'}

interpretOutput' :: [Integer] -> Either (Point2 Integer, Tile) Integer
interpretOutput' output = case output of
  [x, y, tileId] -> if x == (-1) && y == 0
    then Right tileId
    else Left (P2 x y, parseTile tileId)
  _              -> error "Output should be in chunks of three."

parseTile :: Integer -> Tile
parseTile tileId = case tileId of
  0 -> Empty
  1 -> Wall
  2 -> Block
  3 -> Paddle
  4 -> Ball
  _ -> error "Invalid tile id."
