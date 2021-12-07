import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Text (pack)
import Data.Char (digitToInt)
import Data.List

crabs :: Parser [Int]
crabs = fmap read (many digit) `sepBy` char ','

main :: IO ()
main = do
    input <- getContents
    case parse (crabs <* newline) "(source)" (pack input) of
        Left error -> putStr $ show error
        Right crabs -> putStr $ show crabs

