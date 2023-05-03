module Day6 (solve) where
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Text.Parsec         (char, digit, letter, many1, parse, (<|>))
import           Utils.Solution      (Solver)

solve :: Solver
solve input = let
  orbits = parseOrbits input
  part1 = solve1 orbits
  part2 = solve2 orbits
  in (show part1, show part2)

-- Parsing -------------------------

parseOrbits :: String -> [(String, String)]
parseOrbits = map parseOrbit . lines

parseOrbit :: String -> (String, String)
parseOrbit input = case parse orbitParser "" input of
  Left err     -> error $ show err
  Right result -> result
  where
    orbitParser = do
      obj1 <- many1 (letter <|> digit)
      _ <- char ')'
      obj2 <- many1 (letter <|> digit)
      return (obj1, obj2)

-- Part1 --------------------

solve1 :: [(String, String)] -> Integer
solve1 orbits = checksum orbitMap 0 "COM"
  where
    orbitMap = buildOrbitMap orbits

buildOrbitMap :: [(String, String)] -> HashMap String [String]
buildOrbitMap = HM.fromListWith (++) . map (\(k, v) -> (k, [v]))

checksum :: HashMap String [String] -> Integer -> String -> Integer
checksum orbitMap depth object = case HM.lookup object orbitMap of
  Nothing       -> depth
  Just orbitees -> depth + sum (map (checksum orbitMap (depth + 1)) orbitees)

-- Part2 ----------------------------

solve2 :: [(String, String)] -> Integer
solve2 orbits = toInteger $ length youPath + length sanPath - 2 * length samePath - 2
  where
    orbitMap = HM.fromList $ map (\(x, y) -> (y, x)) orbits
    youPath = findPath orbitMap "YOU"
    sanPath = findPath orbitMap "SAN"
    samePath = takeWhile (uncurry (==)) $ zip (reverse youPath) (reverse sanPath)

findPath :: HashMap String String -> String -> [String]
findPath orbitMap object = case HM.lookup object orbitMap of
  Nothing     -> [object]
  Just parent -> object : findPath orbitMap parent
