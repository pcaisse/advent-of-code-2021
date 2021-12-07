import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Text (pack)
import Data.Char (digitToInt)
import Data.List

type Counts = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

fish :: Parser [Int]
fish = fmap digitToInt digit `sepBy` char ','

newDay :: Counts -> Counts
newDay (i0, i1, i2, i3, i4, i5, i6, i7, i8) = (i1, i2, i3, i4, i5, i6, i7 + i0, i8, i0)

simulate :: Counts -> Int -> Int
simulate (i0, i1, i2, i3, i4, i5, i6, i7, i8) 0 = i0 + i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8
simulate counts days = simulate (newDay counts) (days - 1)

fishCounts :: [Int] -> Counts
fishCounts fishes =
    let counts = length <$> (group . sort) fishes
        indexedCounts = [0] ++ counts ++ replicate (8 - length counts) 0
    in case indexedCounts of
        [i0, i1, i2, i3, i4, i5, i6, i7, i8] -> (i0, i1, i2, i3, i4, i5, i6, i7, i8)
        _ -> error "something went wrong"

main :: IO ()
main = do
    input <- getContents
    case parse (fish <* newline) "(source)" (pack input) of
        Left error -> putStr $ show error
        Right fishes -> putStr $ show $ simulate (fishCounts fishes) 256
