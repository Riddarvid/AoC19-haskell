module Day3 (solve) where
import           Data.HashSet   (HashSet)
import qualified Data.HashSet   as HS
import           Data.List      (elemIndex)
import           Text.Parsec    (Parsec, anyChar, char, digit, many1, parse,
                                 sepBy)
import           Utils.Geometry (Point, Vector, distanceFromOrigo, downV, leftV,
                                 move, origo, rightV, upV)
import           Utils.Solution (Solver)

solve :: Solver
solve input = let
  wires = parseWires input
  part1 = uncurry solve1 wires
  part2 = uncurry solve2 wires
  in (show part1, show part2)

data Direction = U | R | D | L
  deriving (Read)

data Instruction = I Direction Integer

type Wire = [Instruction]

parseWires :: String -> (Wire, Wire)
parseWires input = case lines input of
  [str1, str2] -> (parseWire str1, parseWire str2)
  _            -> error "Wrong number of lines in input file"

parseWire :: String -> Wire
parseWire input = case parse wireParser "" input of
  Left err     -> error $ show err
  Right result -> result

wireParser :: Parsec String () Wire
wireParser = instructionParser `sepBy` char ','

instructionParser :: Parsec String () Instruction
instructionParser = do
  direction <- directionParser
  n <- read <$> many1 digit
  return $ I direction n

directionParser :: Parsec String () Direction
directionParser = do
  dirChar <- anyChar
  return $ case dirChar of
    'U' -> U
    'R' -> R
    'D' -> D
    'L' -> L
    _   -> error "Illegal character"

-------------------------

solve1 :: Wire -> Wire -> Integer
solve1 wire1 wire2 = minimum $ HS.map distanceFromOrigo intersections
  where
    intersections = findIntersections wire1 wire2

solve2 :: Wire -> Wire -> Integer
solve2 wire1 wire2 = minimum $ HS.map (numberOfSteps wirePath1 wirePath2) intersections
  where
    wirePath1 = wirePath wire1
    wirePath2 = wirePath wire2
    intersections = findIntersections wire1 wire2

numberOfSteps :: [Point Integer] -> [Point Integer] -> Point Integer -> Integer
numberOfSteps path1 path2 point = numberOfSteps' path1 point + numberOfSteps' path2 point

numberOfSteps' :: [Point Integer] -> Point Integer -> Integer
numberOfSteps' path point = case elemIndex point path of
  Nothing -> error "Point must be in list"
  Just i  -> toInteger i

-- General ----------------------------------

findIntersections :: Wire -> Wire -> HashSet (Point Integer)
findIntersections wire1 wire2 = HS.delete (0, 0) intersections
  where
    wirePath1 = wirePath wire1
    wirePath2 = wirePath wire2
    intersections = HS.intersection (HS.fromList wirePath1) (HS.fromList wirePath2)

wirePath :: Wire -> [Point Integer]
wirePath wire = reverse $ foldl moveStep [origo] deltas
  where
    deltas = concatMap instrDeltas wire

moveStep :: [Point Integer] -> Vector Integer -> [Point Integer]
moveStep (x : xs) v = move x v : x : xs
moveStep [] _       = error "Empty list"

instrDeltas :: Instruction -> [Vector Integer]
instrDeltas (I dir n) = replicate (fromInteger n) point
  where
    point = case dir of
      U -> upV
      R -> rightV
      D -> downV
      L -> leftV
