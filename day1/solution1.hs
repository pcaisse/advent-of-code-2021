import Data.Char
import Data.List
import Data.Sequence.Internal.Sorting

countIncreasing :: [Int] -> Int
countIncreasing [x, y] = if y > x then 1 else 0
countIncreasing (x : y : rest) = countIncreasing [x, y] + countIncreasing (y : rest)
countIncreasing _ = 0

sumEvery3 :: [Int] -> [Int]
sumEvery3 [x, y, z] = [x + y + z]
sumEvery3 (x : y : z : rest) = sumEvery3 [x, y, z] ++ sumEvery3 (y : z : rest)
sumEvery3 l = l

main :: IO ()
main = do
    input <- getContents
    let measurements = map (\a -> read a :: Int) (lines input)
    putStr $ show $ sumEvery3 measurements
