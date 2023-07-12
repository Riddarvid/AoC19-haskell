{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores         #-}

module Day14 (solve) where

import           Control.Monad        (unless)
import           Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask)
import           Control.Monad.State  (MonadState, State, gets, modify,
                                       runState)
import           Data.Foldable        (find)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Text.Parsec          (Parsec, char, letter, many1, parse,
                                       sepBy, string)
import           Utils.Parsing        (numberParser)
import           Utils.Search         (binaryMax)
import           Utils.Solution       (Solver)

type Quantity = (String, Integer)

data Product = Prod Integer [Quantity]

type Rules = HashMap String Product

solve :: Solver
solve input = let
  rules = parseInput input
  part1 = solve1 rules
  part2 = solve2 rules
  in (show part1, show part2)

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
solve1 rules = findOreNeeded rules 1

-- Part 2

solve2 :: Rules -> Integer
solve2 rules = binaryMax (lessThanTrillion rules) 0 10_000_000

lessThanTrillion :: Rules -> Integer -> Bool
lessThanTrillion rules nFuel = findOreNeeded rules nFuel <= 1_000_000_000_000

-- General

findOreNeeded :: Rules -> Integer -> Integer
findOreNeeded rules fuel = case HM.lookup "ORE" (msNeeded ms') of
  Nothing -> error "No ORE found."
  Just n  -> n
  where
    ms' = execMaterialMonad reduceToOre rules (makeMS fuel)

-- Monad utils

data MaterialsState = MS {
  msNeeded    :: HashMap String Integer,
  msAvailable :: HashMap String Integer
}

makeMS :: Integer -> MaterialsState
makeMS nFuel = MS {msNeeded = HM.singleton "FUEL" nFuel, msAvailable = HM.empty}

newtype MaterialMonad a = MM (ReaderT Rules (State MaterialsState) a)
  deriving (Functor, Applicative, Monad, MonadReader Rules, MonadState MaterialsState)

runMaterialMonad :: MaterialMonad a -> Rules -> MaterialsState -> (a, MaterialsState)
runMaterialMonad (MM m) = runState . runReaderT m

execMaterialMonad :: MaterialMonad a -> Rules -> MaterialsState -> MaterialsState
execMaterialMonad m rules s = snd $ runMaterialMonad m rules s

-- Monadic operations

reduceToOre :: MaterialMonad ()
reduceToOre = do
  done <- isFullyReduced
  unless done $ do
    reduceSingle
    reduceToOre

isFullyReduced :: MaterialMonad Bool
isFullyReduced = do
  needed <- gets msNeeded
  return $ all (\(mat, n) -> mat == "ORE" || n == 0) (HM.toList needed)

askProduct :: String -> MaterialMonad Product
askProduct mat = do
  rules <- ask
  case HM.lookup mat rules of
    Nothing   -> error ("Material " ++ mat ++ " not found.")
    Just prod -> return prod

increaseNeeded :: Integer -> Quantity -> MaterialMonad ()
increaseNeeded nProds (mat, neededSingle) = do
  let neededInc = nProds * neededSingle
  modify (\s -> s{msNeeded = HM.insertWith (+) mat neededInc (msNeeded s)})

-- 0. Find first reduceable product.
-- 0.5 Find production amount.
-- 1. Decrease need for product to 0. Increase leftovers by prod amount - needed amount.
-- 2. Increase need for ingredients according to production rules and production amount.
-- 3. Spend leftovers.

reduceSingle :: MaterialMonad ()
reduceSingle = do
  (mat, needed) <- findReduceable
  nProds <- findNProds mat needed
  produce mat nProds needed
  pay mat nProds
  balance

findReduceable :: MaterialMonad (String, Integer)
findReduceable = do
  needed <- gets msNeeded
  case find (\(mat, n) -> mat /= "ORE" && n > 0) (HM.toList needed) of
    Nothing       -> error "No reducable material"
    Just quantity -> return quantity

-- Does not change state
findNProds :: String -> Integer -> MaterialMonad Integer
findNProds mat needed = do
  (Prod prodAmountSingle _) <- askProduct mat
  return $ ceiling $ (fromInteger needed :: Double) / fromInteger prodAmountSingle

-- Changes needed and available of mat.
produce :: String -> Integer -> Integer -> MaterialMonad ()
produce mat nProds matNeeded = do
  (Prod prodAmountSingle _) <- askProduct mat
  let leftover = prodAmountSingle * nProds - matNeeded
  modify (\s -> s{
    msNeeded = HM.insert mat 0 (msNeeded s),
    msAvailable = HM.insert mat leftover (msAvailable s)
  })

pay :: String -> Integer -> MaterialMonad ()
pay mat nProds = do
  (Prod _ quantities) <- askProduct mat
  mapM_ (increaseNeeded nProds) quantities

balance :: MaterialMonad ()
balance = do
  needed <- gets msNeeded
  let neededMats = HM.keys needed
  mapM_ balanceMaterial neededMats

balanceMaterial :: String -> MaterialMonad ()
balanceMaterial mat = do
  needed <- gets msNeeded
  available <- gets msAvailable
  let neededMat = HM.lookupDefault 0 mat needed
  let availableMat = HM.lookupDefault 0 mat available
  let neededMat' = if neededMat > availableMat then neededMat - availableMat else 0
  let availableMat' = if neededMat > availableMat then 0 else availableMat - neededMat
  modify (\s -> s{
    msNeeded = HM.insert mat neededMat' needed,
    msAvailable = HM.insert mat availableMat' available
  })
