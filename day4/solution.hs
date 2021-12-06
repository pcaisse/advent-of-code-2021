import Text.Parsec
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (newline)
import Data.Text (pack)
import Data.List
import Data.Maybe

nat :: Parser Int
nat = read `fmap` many1 digit

draws :: Parser [Int]
draws = nat `sepBy` char ','

cardRow :: Parser [Int]
cardRow = count 5 (spaces *> nat <* spaces)

card :: Parser [[Int]]
card = count 5 cardRow

cards :: Parser [[[Int]]]
cards = many card

drawsAndCards :: Parser ([Int], [[[Int]]])
drawsAndCards = do
    draws <- draws
    count 2 newline
    cards <- cards <* eof
    return (draws, cards)

cardWins :: [Int] -> [[Int]] -> Bool
cardWins draws card = any (\series -> length (draws `intersect` series) == 5) (card ++ transpose card)

scoreCard :: [Int] -> [[Int]] -> Int
scoreCard draws card =
    let unmarkedNumbers = filter (`notElem` draws) (concat card)
        lastNumberCalled = last draws
    in sum unmarkedNumbers * lastNumberCalled

onlyNewWins :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
onlyNewWins scores winningScores =
    let winningCardIndices = map fst winningScores
    in filter (\(index, _) -> index `notElem` winningCardIndices) scores

checkWin :: [Int] -> Int -> [[[Int]]] -> [(Int, Int)] -> [(Int, Int)]
checkWin draws n cards winningScores =
    if n > length draws then winningScores else
    let scores = catMaybes [check card index (take n draws) | (index, card) <- zip [0..] cards]
    in checkWin draws (n + 1) cards (winningScores ++ onlyNewWins scores winningScores)
    where
        check card index currentDraws =
            if cardWins currentDraws card then Just (index, scoreCard currentDraws card) else Nothing

main :: IO ()
main = do
    input <- getContents
    case parse drawsAndCards "(source)" (pack input) of
        Left error -> putStr $ show error
        Right (draws, cards) -> putStr $ show $ checkWin draws 5 cards []
