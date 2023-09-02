module Day16 (solve) where
import           AoCUtils.Days (Solver)
import           Data.Char     (digitToInt)

solve :: Solver
solve input = let
  signal = parseSignal input
  part1 = solve1 signal
  in (show part1, "")

parseSignal :: String -> [Int]
parseSignal = map digitToInt . head . lines

solve1 :: [Int] -> String
solve1 signal = concatMap show (take 8 signal')
  where
    signal' = iterate fft signal !! 100

fft :: [Int] -> [Int]
fft signal = map (fftDigit signal) [1 .. length signal]

fftDigit :: [Int] -> Int -> Int
fftDigit signal patternLength = abs (sum (zipWith (*) signal pattern')) `mod` 10
  where
    pattern' = generatePattern patternLength

generatePattern :: Int -> [Int]
generatePattern pLength = tail $ cycle (zeros ++ ones ++ zeros ++ negOnes)
  where
    zeros = replicate pLength 0
    ones = replicate pLength 1
    negOnes = replicate pLength (-1)
