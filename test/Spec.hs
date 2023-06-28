import           Control.Monad.Writer (MonadWriter (tell), Writer, execWriter)
import           Utils.Days           (ExpectedResult, Input, readInput,
                                       readResults, solvers)
import           Utils.Solution       (Solution, Solver)

main :: IO ()
main = do
  putStrLn ""
  let days = [1 .. length solvers]
  runTests False days

runTests :: Bool -> [Int] -> IO ()
runTests onlyFailed days = do
  results <- checkSolutions days
  mapM_ putStrLn $ execWriter $ writeResults onlyFailed results

writeResults :: Bool -> [TestResult] -> Writer [String] ()
writeResults True results  = do
  tell ["Failed:"]
  mapM_ writeIfFailed results
writeResults False results = mapM_ writeResult results

writeIfFailed :: TestResult -> Writer [String] ()
writeIfFailed (TestResult _ Pass Pass) = return ()
writeIfFailed (TestResult _ (NA _) Pass) = return ()
writeIfFailed (TestResult _ Pass (NA _)) = return ()
writeIfFailed (TestResult _ (NA _) (NA _)) = return ()
writeIfFailed (TestResult day r1 r2) = do
  tell ["\nDay " ++ show day]
  writePartIfFailed r1
  writePartIfFailed r2

writePartIfFailed :: PartResult -> Writer [String] ()
writePartIfFailed (Fail e a) = tell ["Part 1: expected " ++ e ++ ", got " ++ a]
writePartIfFailed _          = return ()

writeResult :: TestResult -> Writer [String] ()
writeResult (TestResult day r1 r2) = do
  tell ["\nDay " ++ show day]
  writePart r1
  writePart r2

writePart :: PartResult -> Writer [String] ()
writePart Pass       = tell ["Part 1: Passed"]
writePart (Fail e a) = tell ["Part 1: expected " ++ e ++ ", got " ++ a]
writePart (NA a)     = tell ["Part 1: result is N/A, got\n" ++ a]

--------------------------- Test running --------------------------------

data PartResult = Pass | Fail String String | NA String
data TestResult = TestResult Int PartResult PartResult

checkSolutions :: [Int] -> IO [TestResult]
checkSolutions = traverse checkSolution

checkSolution :: Int -> IO TestResult
checkSolution day = do
  let solver = solvers !! (day - 1)
  input <- readInput day
  expectedResult <- readResults day
  let (r1, r2) = checkResult solver input expectedResult
  return (TestResult day r1 r2)

checkResult :: Solver -> Input -> ExpectedResult -> (PartResult, PartResult)
checkResult solver input = checkResult' (solver input)

checkResult' :: Solution -> ExpectedResult -> (PartResult, PartResult)
checkResult' (s1, s2) (r1, r2) = let
  c1 = maybeEquals s1 r1
  c2 = maybeEquals s2 r2
  in (c1, c2)

maybeEquals :: String -> Maybe String -> PartResult
maybeEquals s r = case r of
  Nothing -> NA s
  Just r' -> if s == r'
    then Pass
    else Fail r' s
