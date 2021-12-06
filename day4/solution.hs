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

checkWin :: [Int] -> Int -> [[[Int]]] -> Int
checkWin draws n cards =
    if n > length draws then error "no winning cards" else
    case catMaybes [check card (take n draws) | card <- cards] of
        [] -> checkWin draws (n + 1) cards
        (score : _) -> score
    where
        check card currentDraws =
            if cardWins currentDraws card then Just (scoreCard currentDraws card) else Nothing

main :: IO ()
main = do
    input <- getContents
    case parse drawsAndCards "(source)" (pack input) of
        Left error -> putStr $ show error
        Right (draws, cards) -> putStr $ show $ checkWin draws 5 cards
