{-# LANGUAGE TupleSections #-}
import Text.Parsec
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (newline)
import Data.Text (pack)
import Data.List

nat :: Parser Int
nat = read `fmap` many1 digit

coord :: Parser (Int, Int)
coord = do
    x <- nat
    char ','
    y <- nat
    return (x, y)

vent :: Parser ((Int, Int), (Int, Int))
vent = do
    coordA <- coord
    string " -> "
    coordB <- coord
    newline
    return (coordA, coordB)

toCoords :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
toCoords = concatMap fillIn
    where
        fillIn ((x1, y1), (x2, y2))
            | x1 == x2 = map (x1,) (if y2 > y1 then [y1..y2] else [y2..y1])
            | y1 == y2 = map (,y1) (if x2 > x1 then [x1..x2] else [x2..x1])
        fillIn r = []

countOverlaps :: [((Int, Int), (Int, Int))] -> Int
countOverlaps vents =
    let coords = toCoords vents
    in length $ filter (\l -> length l >= 2) (group $ sort coords)

main :: IO ()
main = do
    input <- getContents
    case parse (many1 vent) "(source)" (pack input) of
        Left error -> putStr $ show error
        Right vents -> putStr $ show $ countOverlaps vents
