import Data.Bits
import Numeric

parseBinary :: String -> Int
parseBinary x = case readBin x of
    [(int,_)] -> int
    _ -> error "bad input"

bits :: Int -> [Int] -> [Int]
bits n ints = map (\x -> if x .&. bit (n - 1) /= 0 then 1 else 0) ints

bitListToDec :: [Int] -> Int
bitListToDec bits =
    let zippedBits = zip [0..] (reverse bits)
    in foldr sumHelper 0 zippedBits
    where
       sumHelper (i, value) acc = acc + value * 2 ^ i

main :: IO ()
main = do
    input <- getContents
    let allLines = lines input
    let numDigits = length $ head allLines
    let numLines = length allLines
    let ints = map parseBinary allLines
    let columnBits = [bits i ints | i <- reverse [1..numDigits]]
    let columnOnes = map (filter (== 1)) columnBits
    let bitList = map (\l -> if length l > numLines `div` 2 then 1 else 0) columnOnes
    let gamma = bitListToDec bitList
    let epsilon = bitListToDec (map (xor 1) bitList)
    putStr $ show $ gamma * epsilon
