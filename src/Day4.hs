module Day4 (solve) where
import           AoCUtils.Days (Solver)
import           Text.Parsec   (Parsec, char, digit, many1, parse)

solve :: Solver
solve input = let
  range = parseRange input
  part1 = uncurry solve1 range
  part2 = uncurry solve2 range
  in (show part1, show part2)

-- Parsing ---------------

parseRange :: String -> (Int, Int)
parseRange input = case parse rangeParser "" input of
  Left err     -> error $ show err
  Right result -> result

rangeParser :: Parsec String () (Int, Int)
rangeParser = do
  low <- many1 digit
  _ <- char '-'
  high <- many1 digit
  return (read low, read high)

-- Main -------------------------

solve1 :: Int -> Int -> Int
solve1 low high = length $ filterPasswords low high [not . hasDescending, hasDouble]

solve2 :: Int -> Int -> Int
solve2 low high = length $ filterPasswords low high [not . hasDescending, hasExactDouble]

-- General ----------------------
filterPasswords :: Int -> Int -> [String -> Bool] -> [String]
filterPasswords low high = foldr filter passwords
  where
    passwords = map show [low .. high]

hasDescending :: String -> Bool
hasDescending (x : y : xs)
  | x > y = True
  | otherwise = hasDescending (y : xs)
hasDescending _ = False

hasDouble :: String -> Bool
hasDouble (x : y : xs)
  | x == y = True
  | otherwise = hasDouble (y : xs)
hasDouble _ = False

hasExactDouble :: String -> Bool
hasExactDouble (x : y : z : xs)
  | x == y && x == z = hasExactDouble (dropWhile (== x) xs) -- >2 found, drop all instances
  | x == y = True -- ==2 found, done
  | otherwise = hasExactDouble (y : z : xs)
hasExactDouble [x, y] = x == y -- Only two elements left, has exact double if they are equal
hasExactDouble _ = False
