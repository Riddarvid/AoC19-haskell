{-# LANGUAGE DeriveGeneric #-}

module Day13 (
  solve,
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
import           Utils.Intcode  (Program, evalProgram)
import           Utils.Parsing  (parseICProgram)
import           Utils.Solution (Solver)

data TileType = Empty | Wall | Block | Paddle | Ball
  deriving (Eq, Generic, Show)

instance Hashable TileType

data Tile = Tile (Point2 Integer) TileType
  deriving (Eq, Generic, Show)

instance Hashable Tile

solve :: Solver
solve input = let
  program = parseICProgram input
  part1 = solve1 program
  in (show part1, "")

solve1 :: Program -> Int
solve1 = HS.size . HS.filter (\(Tile _ tileType) -> tileType == Block) . getStartTiles

getStartTiles :: Program -> HashSet Tile
getStartTiles program = interpretOutput $ evalProgram program []

interpretOutput :: [Integer] -> HashSet Tile
interpretOutput = HS.fromList . map interpretOutput' . chunkList 3

interpretOutput' :: [Integer] -> Tile
interpretOutput' output = case output of
  [x, y, tileId] -> Tile (P2 x y) (parseTileType tileId)
  _              -> error "Output should be in chunks of three."

parseTileType :: Integer -> TileType
parseTileType tileId = case tileId of
  0 -> Empty
  1 -> Wall
  2 -> Block
  3 -> Paddle
  4 -> Ball
  _ -> error "Invalid tile id."
