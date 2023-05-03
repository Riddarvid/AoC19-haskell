{-# LANGUAGE InstanceSigs #-}

module Day7 (solve) where
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List           (find, permutations)
import           Data.Maybe          (fromJust)
import           Utils.Intcode       (IcState (Halted), IntcodeComputer,
                                      Program, getIcState, makeIC, runComputer,
                                      setInput)
import           Utils.Parsing       (parseICProgram)
import           Utils.Solution      (Solver)

solve :: Solver
solve input = let
  program = parseICProgram input
  part1 = solve1 program
  part2 = solve2 program
  in (show part1, show part2)

-- An amplifier holds an intcode computer as well as the id of the amplifier supplying its input. It also
-- keeps track of all the output it has generated.
data Amplifier = Amp IntcodeComputer (Maybe String) [Integer]

newtype AmplifierSetup = Amps (HashMap String Amplifier)

instance Show Amplifier where
  show :: Amplifier -> String
  show (Amp comp pre output) = show comp ++ "\nPre: " ++ show pre ++ "\noutput: " ++ show output ++ "\n\n"

instance Show AmplifierSetup where
  show :: AmplifierSetup -> String
  show (Amps amps) = show $ HM.toList amps

solve1 :: Program -> Integer
solve1 program = maximum $ map (runConfiguration1 program) phaseSettings
  where
    phaseSettings = permutations [0..4]

preMap1 :: HashMap String (Maybe String)
preMap1 = HM.insert "A" Nothing $ HM.fromList $ zipWith (\a b -> ([a], Just [b])) ['B'..'E'] ['A'..'D']

runConfiguration1 :: Program -> [Integer] -> Integer
runConfiguration1 program phaseSettings = eOutput endAmps
  where
    startInputs = HM.adjust (++ [0]) "A" $ HM.fromList $ zipWith (\c p -> ([c], [p])) ['A'..'E'] phaseSettings
    startAmps = setupAmplifiers program preMap1 startInputs
    endAmps = fromJust $ find eHasOutput $ iterate stepAmplifiers startAmps

solve2 :: Program -> Integer
solve2 program = maximum $ map (runConfiguration2 program) phaseSettings
  where
    phaseSettings = permutations [5..9]

preMap2 :: HashMap String (Maybe String)
preMap2 = HM.insert "A" (Just "E") preMap1

runConfiguration2 :: Program -> [Integer] -> Integer
runConfiguration2 program phaseSettings = eOutput endAmps
  where
    startInputs = HM.adjust (++ [0]) "A" $ HM.fromList $ zipWith (\c p -> ([c], [p])) ['A'..'E'] phaseSettings
    startAmps = setupAmplifiers program preMap2 startInputs
    endAmps = fromJust $ find eHalted $ iterate stepAmplifiers startAmps

-- General -----------------endAmpss = iterate stepAmplifiers startAmps

setupAmplifiers :: Program -> HashMap String (Maybe String) -> HashMap String [Integer] -> AmplifierSetup
setupAmplifiers program preMap startInputMap = Amps $ HM.mapWithKey (makeAmp program startInputMap) preMap

makeAmp :: Program -> HashMap String [Integer] -> String -> Maybe String -> Amplifier
makeAmp program startInputMap ampId inputId = Amp (makeIC program startInput) inputId []
  where
    startInput = case HM.lookup ampId startInputMap of
      Nothing    -> error "No start input."
      Just input -> input

eHasOutput :: AmplifierSetup -> Bool
eHasOutput (Amps amps) = case HM.lookup "E" amps of
  Nothing                -> error "E is not in amps."
  Just (Amp _ _ (_ : _)) -> True
  _                      -> False

eOutput :: AmplifierSetup -> Integer
eOutput (Amps amps) = case HM.lookup "E" amps of
  Nothing                -> error "E is not in amps."
  Just (Amp _ _ (x : _)) -> x
  _                      -> error "E has no output."

eHalted :: AmplifierSetup -> Bool
eHalted (Amps amps) = case HM.lookup "E" amps of
  Nothing             -> error "E is not in amps."
  Just (Amp comp _ _) -> getIcState comp == Halted

stepAmplifiers :: AmplifierSetup -> AmplifierSetup
stepAmplifiers (Amps amps) = Amps $ connectAmps amps results
  where
    results = HM.map (\(Amp comp _ _) -> runComputer comp) amps

connectAmps :: HashMap String Amplifier -> HashMap String ([Integer], IntcodeComputer) -> HashMap String Amplifier
connectAmps amps results = HM.mapWithKey (connectAmp results) amps

connectAmp :: HashMap String ([Integer], IntcodeComputer) -> String -> Amplifier -> Amplifier
connectAmp results ampId (Amp _ inputId output) = Amp comp'' inputId (output' ++ output)
  where
    (output', comp') = case HM.lookup ampId results of
      Nothing  -> error "Amp not found."
      Just res -> res
    input = case inputId of
      Nothing       -> []
      Just inputId' -> case HM.lookup inputId' results of
        Nothing          -> []
        Just (input', _) -> input'
    comp'' = setInput comp' (reverse input)
