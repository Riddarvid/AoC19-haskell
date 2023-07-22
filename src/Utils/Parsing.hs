module Utils.Parsing (
  parseICProgram
) where
import           AoCUtils.Regex (parseSignedInts)

parseICProgram :: String -> [Integer]
parseICProgram = parseSignedInts
