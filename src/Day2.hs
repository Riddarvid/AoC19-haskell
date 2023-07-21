module Day2 (
  solve
) where
import           AoCUtils.Days (Solver)
import           Data.Foldable (find)
import           Utils.Intcode (Program, execProgram, getMemoryAt)
import           Utils.Parsing (parseICProgram)

solve :: Solver
solve input = let
  program = parseICProgram input
  part1 = solve1 program
  part2 = solve2 program
  in (show part1, show part2)

solve1 :: Program -> Integer
solve1 program = runNounVerb program 12 2

solve2 :: Program -> Integer
solve2 program = 100 * noun + verb
  where
    results = [(noun', verb', runNounVerb program noun' verb') | noun' <- [0..99], verb' <- [0..99]]
    (noun, verb, _) = case find (\(_, _, n) -> n == 19690720) results of
      Nothing     -> error "No correct combination of noun and verb found"
      Just result -> result

runNounVerb :: Program -> Integer -> Integer -> Integer
runNounVerb program noun verb = getMemoryAt (execProgram program' []) 0
  where
    program' = case program of
      (x:_:_:xs) -> x : noun : verb : xs
      _          -> error "Invalid program"
