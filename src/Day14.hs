module Day14 (solve) where
import           Data.Foldable       (find)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Debug.Trace         (traceShow, traceShowId)
import           Text.Parsec         (Parsec, char, letter, many1, parse, sepBy,
                                      string)
import           Utils.Parsing       (numberParser)
import           Utils.Solution      (Solver)

type Quantity = (String, Integer)

data Product = Prod Integer [Quantity]

type Rules = HashMap String Product

type Quantities = HashMap String Integer

solve :: Solver
solve input = let
  requirements = parseInput input
  part1 = solve1 requirements
  in (show part1, "")

-- Parsing

parseInput :: String -> Rules
parseInput = HM.fromList . map parseLine . lines

parseLine :: String -> (String, Product)
parseLine str = case parse requirementParser "" str of
  Left err  -> error $ show err
  Right res -> res

requirementParser :: Parsec String () (String, Product)
requirementParser = do
  preReqs <- quantityParser `sepBy` string ", "
  _ <- string " => "
  (prod, amount) <- quantityParser
  return (prod, Prod amount preReqs)

quantityParser :: Parsec String () Quantity
quantityParser = do
  n <- numberParser
  _ <- char ' '
  material <- many1 letter
  return (material, n)

-- Part 1

solve1 :: Rules -> Integer
solve1 rules = case HM.lookup "ORE" $ reduceToOre rules needed of
  Just n  -> n
  Nothing -> error $ "No ORE found" ++ show (reduceToOre rules needed)
  where
    needed = HM.singleton "FUEL" 1

-- General

reduceToOre :: Rules -> Quantities -> Quantities
reduceToOre rules needed = case find fullyReduced $ map fst $ iterate (uncurry reduce') (needed, HM.empty) of
  Just needed' -> needed'
  Nothing      -> error "Not fully reduced."
  where
    reduce' = reduce rules

fullyReduced :: Quantities -> Bool
fullyReduced = all (\(mat, amount) -> mat == "ORE" || amount == 0) . HM.toList

reduce :: Rules -> Quantities -> Quantities -> (Quantities, Quantities)
reduce rules needed leftover = traceShowId $ case HM.toList needed' of
  []           -> (needed, leftover)
  quantity : _ -> uncurry balance $ pay rules quantity needed leftover
  where
    needed' = HM.filterWithKey (\k _ -> k /= "ORE") needed

pay :: Rules -> Quantity -> Quantities -> Quantities -> (Quantities, Quantities)
pay rules (mat, amount) needed leftover = (needed'', leftover')
  where
    needed' = HM.delete mat needed
    (Prod prodAmount preReqs) = case HM.lookup mat rules of
      Nothing  -> error $ "No rule exists for: " ++ show mat
      Just res -> res
    nApplications = ceiling $ (fromIntegral amount :: Double) / fromIntegral prodAmount
    leftoverAmount = nApplications * prodAmount - amount
    leftover' = HM.insertWith (+) mat leftoverAmount leftover
    preReqs' = map (\(m, n) -> (m, n * nApplications)) preReqs
    needed'' = foldr (uncurry $ HM.insertWith (+)) needed' preReqs'

balance :: Quantities -> Quantities -> (Quantities, Quantities)
balance needed leftover = HM.foldrWithKey balance' (needed, leftover) needed

balance' :: String -> Integer -> (Quantities, Quantities) -> (Quantities, Quantities)
balance' mat amount (needed, leftover) = (needed', leftover')
  where
    leftoverAmount = HM.findWithDefault 0 mat leftover
    reductionAmount = min leftoverAmount amount
    needed' = HM.adjust (\n -> n - reductionAmount) mat needed
    leftover' = HM.insertWith (-) mat reductionAmount leftover
