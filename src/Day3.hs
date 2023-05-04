module Day3 (solve) where
import           Data.HashSet   (HashSet)
import qualified Data.HashSet   as HS
import           Data.List      (elemIndex, find, minimumBy)
import           Data.Maybe     (catMaybes, mapMaybe)
import           Data.Ord       (comparing)
import           Debug.Trace    (trace)
import           Text.Parsec    (Parsec, anyChar, char, digit, many1, parse,
                                 sepBy)
import           Utils.Geometry (Point, Vector, distanceFromOrigo, downV, leftV,
                                 move, origo, rightV, scaleBy, upV)
import           Utils.Solution (Solver)

solve :: Solver
solve input = let
  (instrs1, instrs2) = parseInstructionss input
  wire1 = parseWireSegments instrs1
  wire2 = parseWireSegments instrs2
  part1 = solve1 wire1 wire2
  part2 = solve2 wire1 wire2
  in (show part1, show part2)

data Direction = U | R | D | L
  deriving (Read)

data Instruction = I Direction Integer

data WireSegment = WS (Point Integer) (Point Integer)
  deriving (Show)

type Wire = [WireSegment]

parseWireSegments :: [Instruction] -> Wire
parseWireSegments = reverse . foldl appendWireSegment []

appendWireSegment :: Wire -> Instruction -> Wire
appendWireSegment xs (I dir dist) = WS startPoint endPoint : xs
  where
    startPoint = case xs of
      []           -> origo
      (WS _ p) : _ -> p
    endPoint = move startPoint vector
    vector = case dir of
      U -> scaleBy upV dist
      R -> scaleBy rightV dist
      D -> scaleBy downV dist
      L -> scaleBy leftV dist

parseInstructionss :: String -> ([Instruction], [Instruction])
parseInstructionss input = case lines input of
  [str1, str2] -> (parseInstructions str1, parseInstructions str2)
  _            -> error "Wrong number of lines in input file"

parseInstructions :: String -> [Instruction]
parseInstructions input = case parse instructionsParser "" input of
      Left err     -> error $ show err
      Right result -> result

instructionsParser :: Parsec String () [Instruction]
instructionsParser = instructionParser `sepBy` char ','

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
solve1 wire1 wire2 = minimum $ HS.map distanceFromOrigo intersections --minimum $ HS.map distanceFromOrigo intersections
  where
    intersections = findIntersections wire1 wire2

solve2 :: Wire -> Wire -> Integer
solve2 wire1 wire2 = 5

-- General ----------------------------------

data Intersection = IS (Point Integer) Integer

findIntersections :: Wire -> Wire -> HashSet Intersection
findIntersections wire1 wire2 = HS.delete (0, 0) $ HS.fromList $ foldl (findIntersections' wire2) ([], 0) wire1

findIntersections' :: Wire -> ([Point Integer], Integer) -> WireSegment -> [Point Integer]
findIntersections' xs x = concatMap (findIntersection x) xs

findIntersection :: WireSegment -> WireSegment -> [Point Integer]
findIntersection w1 w2
  | intersects w1 w2 = findIntersection' w1 w2
  | otherwise = []

findIntersection' :: WireSegment -> WireSegment -> [Point Integer]
findIntersection' w1@(WS (startX1, startY1) (endX1, endY1)) w2@(WS (startX2, startY2) (endX2, endY2))
  | isHorizontal w1 && isHorizontal w2 = [(x, startY1) | x <- [max minX1 minX2 .. min maxX1 maxX2]]
  | isVertical w1 && isVertical w2 = [(startX1, y) | y <- [max minY1 minY2 .. min maxY1 maxY2]]
  | isHorizontal w1 && isVertical w2 = [(startX2, startY1)]
  | otherwise = [(startX1, startY2)]
  where
    maxX1 = max startX1 endX1
    minX1 = min startX1 endX1
    maxX2 = max startX2 endX2
    minX2 = min startX2 endX2

    maxY1 = max startY1 endY1
    minY1 = min startY1 endY1
    maxY2 = max startY2 endY2
    minY2 = min startY2 endY2

isHorizontal :: WireSegment -> Bool
isHorizontal (WS (_, y1) (_, y2)) = y1 == y2

isVertical :: WireSegment -> Bool
isVertical = not . isHorizontal

intersects :: WireSegment -> WireSegment -> Bool
intersects (WS (startX1, startY1) (endX1, endY1)) (WS (startX2, startY2) (endX2, endY2)) =
  overlapsX && overlapsY
  where
    maxX1 = max startX1 endX1
    minX1 = min startX1 endX1
    maxX2 = max startX2 endX2
    minX2 = min startX2 endX2
    overlapsX = maxX1 >= minX2 && minX1 <= maxX2
    maxY1 = max startY1 endY1
    minY1 = min startY1 endY1
    maxY2 = max startY2 endY2
    minY2 = min startY2 endY2
    overlapsY = maxY1 >= minY2 && minY1 <= maxY2
