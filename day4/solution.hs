import Text.Parsec
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (newline)
import Data.Text (pack)

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

main :: IO ()
main = do
    input <- getContents
    let result = parse drawsAndCards "(source)" (pack input)
    putStr $ show result
