module Main where

import Text.Parsec

main :: IO ()
main = do
    input <- getContents
    let linesWithContent = filter (/= "") (lines input)
    let draws = head linesWithContent
    let cards = tail linesWithContent
    putStr $ show draws ++ show cards
