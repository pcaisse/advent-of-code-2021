import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Text (pack)
import Data.Char (digitToInt)
import Data.List

crabs :: Parser [Int]
crabs = fmap read (many digit) `sepBy` char ','

average xs = floor $ realToFrac (sum xs) / genericLength xs

fuel :: [Int] -> Int
fuel crabs =
    let avg = average crabs
        dists = map (\pos -> sum [1..dist pos avg]) crabs
    in sum dists
    where
        dist a b = abs (a - b)

main :: IO ()
main = do
    input <- getContents
    case parse (crabs <* newline) "(source)" (pack input) of
        Left error -> putStr $ show error
        Right crabs -> putStr $ show $ fuel crabs

