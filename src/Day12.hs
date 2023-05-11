{-# LANGUAGE InstanceSigs #-}

module Day12 (solve) where
import           Data.Hashable  (Hashable (hashWithSalt))
import           Data.HashSet   (HashSet)
import qualified Data.HashSet   as HS
import           Data.List      (elemIndex)
import           Text.Parsec    (parse, string)
import           Utils.Geometry (Point3 (P3), Vector3, moveBy)
import           Utils.Parsing  (numberParser)
import           Utils.Solution (Solver)

data Moon = Moon (Point3 Integer) (Vector3 Integer)
  deriving (Eq)

instance Hashable Moon where
  hashWithSalt :: Int -> Moon -> Int
  hashWithSalt salt (Moon p v) = hashWithSalt salt (p, v)

solve :: Solver
solve input = let
  moons = parseMoons input
  part1 = solve1 moons
  part2 = solve2 moons
  in (show part1, show part2)

-- Parsing

parseMoons :: String -> [Moon]
parseMoons = map parseMoon . lines

parseMoon :: String -> Moon
parseMoon str = case parse moonParser "" str of
  Left err     -> error $ show err
  Right result -> result
  where
    moonParser = do
      _ <- string "<x="
      x <- numberParser
      _ <- string ", y="
      y <- numberParser
      _ <- string ", z="
      z <- numberParser
      return $ Moon (P3 x y z) (P3 0 0 0)

-- Part 1

solve1 :: [Moon] -> Integer
solve1 moons = totalEnergy (iterate stepMoons moons !! 1000)

totalEnergy :: [Moon] -> Integer
totalEnergy = sum . map totalEnergy'

totalEnergy' :: Moon -> Integer
totalEnergy' (Moon (P3 x y z) (P3 dx dy dz)) = pot * kin
  where
    pot = abs x + abs y + abs z
    kin = abs dx + abs dy + abs dz

-- Part 2

solve2 :: [Moon] -> Integer
solve2 moons = lcm xCycle $ lcm yCycle zCycle
  where
    states = iterate stepMoons moons
    xCycle = cycleLength xPart states
    yCycle = cycleLength yPart states
    zCycle = cycleLength zPart states

cycleLength :: (Moon -> (Integer, Integer)) -> [[Moon]] -> Integer
cycleLength moonPart states = case elemIndex startState $ tail states' of
  Nothing  -> error "Cycle not found"
  Just res -> toInteger res + 1
  where
    startState = head states'
    states' = map (map moonPart) states

xPart :: Moon -> (Integer, Integer)
xPart (Moon (P3 x _ _) (P3 dx _ _)) = (x, dx)

yPart :: Moon -> (Integer, Integer)
yPart (Moon (P3 _ y _) (P3 _ dy _)) = (y, dy)

zPart :: Moon -> (Integer, Integer)
zPart (Moon (P3 _ _ z) (P3 _ _ dz)) = (z, dz)

takeUnique :: (Hashable a) =>[a] -> [a]
takeUnique = takeUnique' HS.empty

takeUnique' :: Hashable a => HashSet a -> [a] -> [a]
takeUnique' _ [] = []
takeUnique' unique (x : xs)
  | HS.member x unique = []
  | otherwise = x : takeUnique' (HS.insert x unique) xs

-- General

stepMoons :: [Moon] -> [Moon]
stepMoons moons = moons''
  where
    moons' = map (applyGravity moons) moons
    moons'' = map applyVelocity moons'

applyGravity :: [Moon] -> Moon -> Moon
applyGravity moons moon@(Moon pos velocity) = Moon pos velocity'
  where
    velocity' = foldr (moveBy . gravityVector moon) velocity moons

gravityVector :: Moon -> Moon -> Point3 Integer
gravityVector (Moon pStart _) (Moon pEnd _) = f <$> pStart <*> pEnd
  where
    f cStart cEnd = case compare cStart cEnd of
      LT -> 1
      EQ -> 0
      GT -> (-1)

applyVelocity :: Moon -> Moon
applyVelocity (Moon p v) = Moon (moveBy p v) v
