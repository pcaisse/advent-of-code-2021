import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Text (pack)
import Data.Char (digitToInt)

fish :: Parser [Int]
fish = fmap digitToInt digit `sepBy` char ','

newDay :: [Int] -> [Int]
newDay fishes =
    let numReproducingFish = length $ filter (== 0) fishes
        newFish = replicate numReproducingFish 8
    in map nextFish fishes ++ newFish
    where
        nextFish 0 = 6
        nextFish x = x - 1

simulate :: [Int] -> Int -> [Int]
simulate fishes 0 = fishes
simulate fishes days = simulate (newDay fishes) (days - 1)

main :: IO ()
main = do
    input <- getContents
    case parse (fish <* newline) "(source)" (pack input) of
        Left error -> putStr $ show error
        Right fishes -> putStr $ show $ length $ simulate fishes 80
