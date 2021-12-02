import Data.Char
import Data.List
import Data.Sequence.Internal.Sorting

countIncreasing :: [Int] -> Int
countIncreasing [x, y] = if y > x then 1 else 0
countIncreasing (x : y : rest) = countIncreasing [x, y] + countIncreasing (y : rest)
countIncreasing _ = 0

main :: IO ()
main = do
    input <- getContents
    let measurements = map (\a -> read a :: Int) (lines input)
    putStr $ show $ countIncreasing measurements
