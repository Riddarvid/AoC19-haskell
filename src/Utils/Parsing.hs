module Utils.Parsing (
  parseICProgram
) where
import           AoCUtils.Parsing (numberParser)
import           Text.Parsec      (Parsec, char, parse, sepBy)

parseICProgram :: String -> [Integer]
parseICProgram input = case parse icParser "" input of
  Left err      -> error $ show err
  Right program -> program

icParser :: Parsec String () [Integer]
icParser = numberParser `sepBy` char ','
