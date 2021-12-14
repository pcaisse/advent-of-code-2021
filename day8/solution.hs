import Text.Parsec
import Text.Parsec.Char (oneOf, spaces)
import Text.Parsec.Text (Parser)
import Data.Text (pack)
import Data.List
import Data.Ord

type Entry = (Observations, Output)

newtype Observations = Observations [String] deriving (Show)
newtype Output = Output [String] deriving (Show)

newtype Digits = Digits (String, String, String, String, String, String, String, String, String, String) deriving Show

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

filterByLength :: Int -> [String] -> String
filterByLength n obs = head $ filter (\o -> length o == n) obs

-- ["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"] ->
-- ["aaaa","gggggg","ccccccc","fffffff","bbbbbbbb","dddddddd","eeeeeeeee"]
groupedSortedLetters :: [String] -> [String]
groupedSortedLetters = sortOn length . group . sort . concat

-- deduction logic:
-- a = 7 - 1
-- b = shared by 6 letters
-- e = shared by 4 letters
-- f = shared by 9 letters
-- c = 1 - f
-- d = 4 - cbf
-- g = last remaining letter
decode :: Entry -> Int
decode (Observations obs, Output out) =
    let one = sort $ filterByLength 2 obs
        four = sort $ filterByLength 4 obs
        seven = sort $ filterByLength 3 obs
        eight = "abcdefg"
        a = head $ seven \\ one
        grouped = groupedSortedLetters obs
        b = head $ filterByLength 6 grouped
        e = head $ filterByLength 4 grouped
        f = head $ filterByLength 9 grouped
        c = head $ one \\ [f]
        d = head $ four \\ [c, b, f]
        g = head $ eight \\ [a, b, c, d, e, f]
        zero = sort [a, b, c, e, f, g]
        two = sort [a, c, d, e, g]
        three = sort [a, c, d, f, g]
        five = sort [a, b, d, f, g]
        six = sort [a, b, d, e, f, g]
        nine = sort [a, b, c, d, f, g]
        digits = Digits (zero, one, two, three, four, five, six, seven, eight, nine)
    in read $ concatMap (decodeDigit digits . sort) out
    where
        decodeDigit (Digits (zero, one, two, three, four, five, six, seven, eight, nine)) digit
            | digit == zero = "0"
            | digit == one = "1"
            | digit == two = "2"
            | digit == three = "3"
            | digit == four = "4"
            | digit == five = "5"
            | digit == six = "6"
            | digit == seven = "7"
            | digit == eight = "8"
        decodeDigit _ _ = "9"

main :: IO ()
main = do
    input <- getContents
    case parse (many1 entry <* eof) "(source)" (pack input) of
        Left error -> putStr $ show error
        Right entries -> putStr $ show $ sum $ map decode entries
