import           AoCUtils.Days        (ExpectedResult, Input, Solver, Solution,
                                       readInput, readResults)
import Utils.Days (solvers)
import           Control.Monad        (when)
import           Control.Monad.Writer (MonadWriter (tell), Writer, execWriter)

main :: IO ()
main = do
  putStrLn ""
  let days = [1 .. length solvers]
  runTests True days

runTests :: Bool -> [Int] -> IO ()
runTests verbose days = do
  results <- checkSolutions days
  when verbose (printResults False results)
  putStrLn "\n\n-----------------------------\n\n"
  if all succeeded results
    then do
      putStrLn "All tests succeeded!"
    else do
      putStrLn "One or more tests failed:"
      printResults True results

printResults :: Bool -> [TestResult] -> IO ()
printResults onlyFailed results = mapM_ putStrLn $ execWriter $ writeResults onlyFailed results

writeResults :: Bool -> [TestResult] -> Writer [String] ()
writeResults = mapM_ . writeResult

writeResult :: Bool -> TestResult -> Writer [String] ()
writeResult onlyFailed res@(TestResult day r1 r2) = when (writeAll || testFailed res) $ do
  tell ["\nDay " ++ show day]
  when (writeAll || partFailed r1) $ writePart 1 r1
  when (writeAll || partFailed r2) $ writePart 2 r2
  where
    writeAll = not onlyFailed

writePart :: Int -> PartResult -> Writer [String] ()
writePart day res = tell [start ++ end]
  where
    start = "Part " ++ show day ++ ": "
    end = case res of
      Pass     -> "Passed"
      Fail e a -> "Expected " ++ e ++ ", got " ++ a
      NA a     -> "Result is N/A, got\n" ++ a

partFailed :: PartResult -> Bool
partFailed (Fail _ _) = False
partFailed _          = True

testFailed :: TestResult -> Bool
testFailed (TestResult _ p1 p2) = partFailed p1 || partFailed p2

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

succeeded :: TestResult -> Bool
succeeded (TestResult _ (Fail _ _) _) = False
succeeded (TestResult _ _ (Fail _ _)) = False
succeeded _                           = True
