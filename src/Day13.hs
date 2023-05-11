{-# LANGUAGE DeriveGeneric #-}

module Day13 (
  solve,
  GameState(..),
  TileType(..),
  Tile(..),
  getStartTiles
) where
import           Data.Hashable  (Hashable)
import           Data.HashSet   (HashSet)
import qualified Data.HashSet   as HS
import           GHC.Generics   (Generic)
import           GHC.Utils.Misc (chunkList)
import           Utils.Geometry (Point2 (P2))
import           Utils.Intcode  (IntcodeComputer, Program, evalProgram)
import           Utils.Parsing  (parseICProgram)
import           Utils.Solution (Solver)

data TileType = Empty | Wall | Block | Paddle | Ball
  deriving (Eq, Generic, Show)

instance Hashable TileType

data Tile = Tile (Point2 Integer) TileType
  deriving (Eq, Generic, Show)

instance Hashable Tile

data GameState = GS {
  gsTiles :: HashSet Tile,
  gsScore :: Integer
}

emptyGS :: GameState
emptyGS = GS {gsTiles = HS.empty, gsScore = 0}

solve :: Solver
solve input = let
  program = parseICProgram input
  part1 = solve1 program
  in (show part1, "")

-- Part 1

solve1 :: Program -> Int
solve1 = HS.size . HS.filter (\(Tile _ tileType) -> tileType == Block) . gsTiles . getStartTiles

getStartTiles :: Program -> GameState
getStartTiles program = interpretOutput $ evalProgram program []

-- Part 2

solve2 :: Program -> Integer
solve2 = undefined

playGame :: IntcodeComputer -> (HashSet Tile, Integer)
playGame ic = undefined

-- General

interpretOutput :: [Integer] -> GameState
interpretOutput = foldl appendGS emptyGS . chunkList 3

appendGS :: GameState -> [Integer] -> GameState
appendGS gs output = case interpretOutput' output of
  Left tile    -> gs{gsTiles = HS.insert tile (gsTiles gs)}
  Right score' -> gs{gsScore = score'}

interpretOutput' :: [Integer] -> Either Tile Integer
interpretOutput' output = case output of
  [x, y, tileId] -> if x == (-1) && y == 0
    then Right tileId
    else Left $ Tile (P2 x y) (parseTileType tileId)
  _              -> error "Output should be in chunks of three."

parseTileType :: Integer -> TileType
parseTileType tileId = case tileId of
  0 -> Empty
  1 -> Wall
  2 -> Block
  3 -> Paddle
  4 -> Ball
  _ -> error "Invalid tile id."
