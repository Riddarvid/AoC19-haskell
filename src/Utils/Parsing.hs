module Utils.Parsing (
  parseICProgram
) where
import           Text.Parsec (Parsec, char, digit, many1, parse, sepBy)

parseICProgram :: String -> [Integer]
parseICProgram input = case parse icParser "" input of
  Left err      -> error $ show err
  Right program -> program

icParser :: Parsec String () [Integer]
icParser = numberParser `sepBy` char ','

numberParser :: Parsec String () Integer
numberParser = read <$> many1 digit
