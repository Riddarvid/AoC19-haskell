{-# LANGUAGE InstanceSigs #-}

module Utils.Intcode (
  IntcodeComputer,
  Program,
  IcState(..),
  makeIC,
  runComputer,
  runProgram,
  evalProgram,
  execProgram,
  getMemoryAt,
  getOutput,
  getIcState,
  setInput,
  isHalted,
  isWaiting
) where

import           Control.Monad              (unless, when)
import           Control.Monad.State.Strict (State, gets, modify, runState)
import           Data.IntMap.Strict         (IntMap, (!))
import qualified Data.IntMap.Strict         as IntMap

data IntcodeComputer = IC {
  icMemory  :: IntMap Integer,
  icPointer :: Int,
  icModeNum :: Int,
  icInput   :: [Integer],
  icOutput  :: [Integer],
  icState   :: IcState,
  icBase    :: Int
}

instance Show IntcodeComputer where
  show :: IntcodeComputer -> String
  show comp = "Comp, in: " ++ show (icInput comp) ++ ", out: " ++ show (icOutput comp) ++ ", " ++ show (icState comp)

data IcState = Running | Halted | Waiting
  deriving (Eq, Show)

data ParameterMode = Immediate | Position | Relative

type Program = [Integer]

makeIC :: Program -> [Integer] -> IntcodeComputer
makeIC memory input = IC {
  icMemory = IntMap.fromList $ zip [0 ..] memory,
  icPointer = 0,
  icModeNum = 0,
  icInput = input,
  icOutput = [],
  icState = Running,
  icBase = 0
}

-- Sets the input of the computer, resets output and sets state to running if it was waiting.
setInput :: IntcodeComputer -> [Integer] -> IntcodeComputer
setInput comp input = comp{
  icInput = input,
  icOutput = [],
  icState = state'}
  where
    state' = case icState comp of
      Waiting -> Running
      state'' -> state''

runProgram :: Program -> [Integer] -> ([Integer], IntcodeComputer)
runProgram program = runComputer . makeIC program

execProgram :: Program -> [Integer] -> IntcodeComputer
execProgram program = snd . runProgram program

evalProgram :: Program -> [Integer] -> [Integer]
evalProgram program = fst . runProgram program

-- Operations

runComputer :: IntcodeComputer -> ([Integer], IntcodeComputer)
runComputer = runState execProgramState

execProgramState :: State IntcodeComputer [Integer]
execProgramState = do
  icState' <- gets icState
  if icState' == Halted || icState' == Waiting
    then gets icOutput
    else do
      opCode <- readImmediate
      let (mode, opCode') = parseModeOp opCode
      setModeNum mode
      case opCode' of
        99 -> haltOp
        1  -> addOp
        2  -> mulOp
        3  -> inputOp
        4  -> outputOp
        5  -> jumpIfTrueOp
        6  -> jumpIfFalseOp
        7  -> lessThanOp
        8  -> equalsOp
        9  -> adjustBaseOp
        _  -> error $ show opCode' ++ " not yet implemented."
      execProgramState

haltOp :: State IntcodeComputer ()
haltOp = modify (\s -> s{icState = Halted})

addOp :: State IntcodeComputer ()
addOp = do
  op1 <- readParameter
  op2 <- readParameter
  writeParameter (op1 + op2)

mulOp :: State IntcodeComputer ()
mulOp = do
  op1 <- readParameter
  op2 <- readParameter
  writeParameter (op1 * op2)

inputOp :: State IntcodeComputer ()
inputOp = do
  input <- consumeInput
  case input of
    Nothing -> do
      modify (\s -> s{icPointer = icPointer s - 1}) -- Reset instruction pointer to the state it had before.
      modify (\s -> s{icState = Waiting})
    Just input' -> writeParameter input'

outputOp :: State IntcodeComputer ()
outputOp = do
  val <- readParameter
  appendOutput val

jumpIfTrueOp :: State IntcodeComputer ()
jumpIfTrueOp = do
  cond <- readParameter
  address <- fromInteger <$> readParameter
  unless (cond == 0) (setPointer address)

jumpIfFalseOp :: State IntcodeComputer ()
jumpIfFalseOp = do
  cond <- readParameter
  address <- fromInteger <$> readParameter
  when (cond == 0) (setPointer address)

lessThanOp :: State IntcodeComputer ()
lessThanOp = do
  val1 <- readParameter
  val2 <- readParameter
  writeParameter (if val1 < val2 then 1 else 0)

equalsOp :: State IntcodeComputer ()
equalsOp = do
  val1 <- readParameter
  val2 <- readParameter
  writeParameter (if val1 == val2 then 1 else 0)

adjustBaseOp :: State IntcodeComputer ()
adjustBaseOp = do
  offset <- fromInteger <$> readParameter
  adjustBase offset

-- General utils for interacting with IntcodeComputer state -----------------------------

-- Set pointer to the given value
setPointer :: Int -> State IntcodeComputer ()
setPointer p = modify (\s -> s{icPointer = p})

-- Increments pointer by 1
incPointer :: State IntcodeComputer ()
incPointer = modify (\s -> s{icPointer = icPointer s + 1})

-- Reads the memory at the given address, without modifying pointer.
readValue :: Int -> State IntcodeComputer Integer
readValue address = do
  memory <- gets icMemory
  return $ IntMap.findWithDefault 0 address memory

-- Writes a value to the given address, without modifying the pointer.
writeValue :: Int -> Integer -> State IntcodeComputer ()
writeValue address value = modify (\s -> s{icMemory = IntMap.insert address value $ icMemory s})

setModeNum :: Int -> State IntcodeComputer ()
setModeNum mode = modify (\s -> s{icModeNum = mode})

-- Retrieves the mode to be used for the next parameter. Shifts the mode one position to the right.
getParameterMode :: State IntcodeComputer ParameterMode
getParameterMode = do
  modeNum <- gets icModeNum
  let mode = parseMode (modeNum `mod` 10)
  modify (\s -> s{icModeNum = modeNum `div` 10})
  return mode

consumeInput :: State IntcodeComputer (Maybe Integer)
consumeInput = do
  input <- gets icInput
  case input of
    []       -> return Nothing
    (x : xs) -> do
      modify (\s -> s{icInput = xs})
      return $ Just x

appendOutput :: Integer -> State IntcodeComputer ()
appendOutput val = modify (\s -> s{icOutput = icOutput s ++ [val]})

adjustBase :: Int -> State IntcodeComputer ()
adjustBase offset = modify (\s -> s{icBase = icBase s + offset})

-- Functions for interacting with parameters ------------------------------------------

readParameter :: State IntcodeComputer Integer
readParameter = do
  mode <- getParameterMode
  case mode of
    Immediate -> readImmediate
    Position  -> readPosition
    Relative  -> readRelative

-- Reads a parameter using immediate addressing. Increments pointer.
readImmediate :: State IntcodeComputer Integer
readImmediate = do
  pointer <- gets icPointer
  val <- readValue pointer
  incPointer
  return val

-- Reads a parameter using position addressing. Increments pointer.
readPosition :: State IntcodeComputer Integer
readPosition = do
  pointer <- gets icPointer
  address <- fromInteger <$> readValue pointer
  val <- readValue address
  incPointer
  return val

readRelative :: State IntcodeComputer Integer
readRelative = do
  pointer <- gets icPointer
  address <- fromInteger <$> readValue pointer
  base <- gets icBase
  val <- readValue (address + base)
  incPointer
  return val

writeParameter :: Integer -> State IntcodeComputer ()
writeParameter val = do
  mode <- getParameterMode
  case mode of
    Immediate -> error "Writes cannot use immediate addressing."
    Position  -> writePosition val
    Relative  -> writeRelative val

writePosition :: Integer -> State IntcodeComputer ()
writePosition val = do
  pointer <- gets icPointer
  address <- fromInteger <$> readValue pointer
  writeValue address val
  incPointer

writeRelative :: Integer -> State IntcodeComputer ()
writeRelative val = do
  pointer <- gets icPointer
  address <- fromInteger <$> readValue pointer
  base <- gets icBase
  writeValue (address + base) val
  incPointer

-- General utils not using state -------------------------

parseModeOp :: Integer -> (Int, Int)
parseModeOp opCode = (fromInteger $ opCode `div` 100, fromInteger $ opCode `mod` 100)

parseMode :: Int -> ParameterMode
parseMode n = case n of
  0 -> Position
  1 -> Immediate
  2 -> Relative
  _ -> error $ "No such parameter mode: " ++ show n

-- Getters

getMemoryAt :: IntcodeComputer -> Int -> Integer
getMemoryAt = (!) . icMemory

getOutput :: IntcodeComputer -> [Integer]
getOutput = icOutput

getIcState :: IntcodeComputer -> IcState
getIcState = icState

isHalted :: IntcodeComputer -> Bool
isHalted ic = icState ic == Halted

isWaiting :: IntcodeComputer -> Bool
isWaiting ic = icState ic == Waiting
