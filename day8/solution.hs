import Text.Parsec
import Text.Parsec.Char (oneOf, spaces)
import Text.Parsec.Text (Parser)
import Data.Text (pack)

type Entry = (Observations, Output)

newtype Observations = Observations [String]
newtype Output = Output [String]

segmentGroup :: Parser String
segmentGroup = many1 $ oneOf "abcdefg"

observations :: Parser Observations
observations = Observations <$> count 10 (spaces *> segmentGroup <* spaces)

output :: Parser Output
output = Output <$> count 4 (spaces *> segmentGroup <* spaces)

entry :: Parser Entry
entry = do
  part1 <- observations
  char '|'
  part2 <- output
  return (part1, part2)

sumUnique :: Int -> Output -> Int
sumUnique acc (Output out) = acc + length (filterUnique out)
  where
    filterUnique = filter (\o -> length o == 2 || length o == 4 || length o == 3 || length o == 7)

countUnique :: [Entry] -> Int
countUnique entries = foldl sumUnique 0 (map snd entries)

main :: IO ()
main = do
    input <- getContents
    case parse (many1 entry <* eof) "(source)" (pack input) of
        Left error -> putStr $ show error
        Right entries -> putStr $ show $ countUnique entries
