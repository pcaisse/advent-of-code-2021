import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Text (pack)
import Data.Char (digitToInt)
import Data.List

crabs :: Parser [Int]
crabs = fmap read (many digit) `sepBy` char ','

median :: [Int] -> Int
median sortedLs = sortedLs !! (length sortedLs `div` 2)

fuel :: [Int] -> Int
fuel crabs =
    let sortedCrabs = sort crabs
        med = median sortedCrabs
    in sum $ map (\pos -> abs $ pos - med) sortedCrabs

main :: IO ()
main = do
    input <- getContents
    case parse (crabs <* newline) "(source)" (pack input) of
        Left error -> putStr $ show error
        Right crabs -> putStr $ show $ fuel crabs

