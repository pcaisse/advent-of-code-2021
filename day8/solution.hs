import Text.Parsec
import Text.Parsec.Char (oneOf, spaces)
import Text.Parsec.Text (Parser)
import Data.Text (pack)

type Entry = ([String], [String])

segmentGroup :: Parser String
segmentGroup = many1 $ oneOf "abcdefg"

observations :: Parser [String]
observations = count 10 (spaces *> segmentGroup <* spaces)

output :: Parser [String]
output = count 4 (spaces *> segmentGroup <* spaces)

entry :: Parser Entry
entry = do
  part1 <- observations
  char '|'
  part2 <- output
  return (part1, part2)

main :: IO ()
main = do
    input <- getContents
    case parse (many1 entry <* eof) "(source)" (pack input) of
        Left error -> putStr $ show error
        Right entries -> putStr $ show entries

