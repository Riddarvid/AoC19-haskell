module Utils.Parsing (
  parseICProgram,
  numberParser
) where
import           Text.Parsec (Parsec, char, digit, many1, optionMaybe, parse,
                              sepBy)

parseICProgram :: String -> [Integer]
parseICProgram input = case parse icParser "" input of
  Left err      -> error $ show err
  Right program -> program

icParser :: Parsec String () [Integer]
icParser = numberParser `sepBy` char ','

numberParser :: Parsec String () Integer
numberParser = do
  sign <- optionMaybe (char '-')
  num <- read <$> many1 digit
  return $ case sign of
    Nothing -> num
    Just _  -> (-num)
