module Utils.Intcode (
  IntcodeComputer,
  Program,
  makeIC,
  execProgram,
  getMemoryAt
) where

import           Control.Monad.State.Strict (State, execState, gets, modify,
                                             unless)
import           Data.IntMap.Strict         (IntMap, (!))
import qualified Data.IntMap.Strict         as IntMap

data IntcodeComputer = IC {
  icMemory  :: IntMap Integer,
  icPointer :: Int,
  icHalted  :: Bool
}

type Program = [Integer]

makeIC :: Program -> IntcodeComputer
makeIC memory = IC {
  icMemory = IntMap.fromList $ zip [0 ..] memory,
  icPointer = 0,
  icHalted = False
}

execProgram :: Program -> IntcodeComputer
execProgram = execComputer . makeIC

-- Operations

execComputer :: IntcodeComputer -> IntcodeComputer
execComputer = execState execProgramState

execProgramState :: State IntcodeComputer ()
execProgramState = do
  halted <- gets icHalted
  unless halted $ do
    opCode <- readImmediate
    case opCode of
      99 -> haltOp
      1  -> addOp
      2  -> mulOp
      _  -> error $ show opCode ++ " not yet implemented."
    execProgramState

haltOp :: State IntcodeComputer ()
haltOp = modify (\s -> s{icHalted = True})

addOp :: State IntcodeComputer ()
addOp = do
  op1 <- readDirect
  op2 <- readDirect
  writeDirect (op1 + op2)

mulOp :: State IntcodeComputer ()
mulOp = do
  op1 <- readDirect
  op2 <- readDirect
  writeDirect (op1 * op2)

-- Utils

setPointer :: Int -> State IntcodeComputer ()
setPointer p = modify (\s -> s{icPointer = p})

incPointer :: State IntcodeComputer ()
incPointer = modify (\s -> s{icPointer = icPointer s + 1})

readValue :: Int -> State IntcodeComputer Integer
readValue address = do
  memory <- gets icMemory
  return (memory ! address)

writeValue :: Int -> Integer -> State IntcodeComputer ()
writeValue address value = modify (\s -> s{icMemory = IntMap.insert address value $ icMemory s})

readImmediate :: State IntcodeComputer Integer
readImmediate = do
  pointer <- gets icPointer
  val <- readValue pointer
  incPointer
  return val

-- Reads the value at the address of the instruction pointer. Then reads the value at the address specified
-- by that value. Increments ip.
readDirect :: State IntcodeComputer Integer
readDirect = do
  pointer <- gets icPointer
  address <- fromInteger <$> readValue pointer
  val <- readValue address
  incPointer
  return val

writeDirect :: Integer -> State IntcodeComputer ()
writeDirect val = do
  pointer <- gets icPointer
  address <- fromInteger <$> readValue pointer
  writeValue address val
  incPointer

-- Getters

getMemoryAt :: IntcodeComputer -> Int -> Integer
getMemoryAt = (!) . icMemory
