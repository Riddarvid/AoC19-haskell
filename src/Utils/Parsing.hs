module Utils.Parsing (
  parseICProgram
) where
import           AoCUtils.Parsing (parseSignedInts)
import           Data.Maybe       (fromJust)

parseICProgram :: String -> [Integer]
parseICProgram = parseSignedInts
