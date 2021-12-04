import Data.Bits
import Numeric

parseBinary :: String -> Int
parseBinary x = case readBin x of
    [(int,_)] -> int
    _ -> error "bad input"

bitAtIndex :: Int -> Int -> Int -> Int
bitAtIndex numDigits index int = if int .&. bit (numDigits - 1 - index) /= 0 then 1 else 0

-- return all bits for one column in matrix by offset (0 is first bit)
bitsByColumnIndex :: Int -> Int -> [Int] -> [Int]
bitsByColumnIndex numDigits offset ints = map (\x -> bitAtIndex numDigits offset x) ints

findRating :: Int -> Int -> Bool -> [Int] -> Int
findRating numDigits index most [] = error "something went wrong"
findRating numDigits index most [int] = int
findRating numDigits index most ints =
    let bits = bitsByColumnIndex numDigits index ints
        ones = filter (== 1) bits
        zeroes = filter (== 0) bits
        mostCommonValue = if length ones >= length zeroes then 1 else 0
        leastCommonValue = if length ones >= length zeroes then 0 else 1
        value = if most then mostCommonValue else leastCommonValue
        intsWithCommonValue = filter (\int -> bitAtIndex numDigits index int == value) ints
    in findRating numDigits (index + 1) most intsWithCommonValue

main :: IO ()
main = do
    input <- getContents
    let ints = map parseBinary (lines input)
    let numDigits :: Int = ceiling $ logBase 2 $ fromIntegral $ maximum ints
    let oxygen = findRating numDigits 0 True ints
    let co2 = findRating numDigits 0 False ints
    putStr $ show $ oxygen * co2
