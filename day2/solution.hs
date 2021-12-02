{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
import Data.Char
import Data.List

data Movement = Down Int | Up Int | Forward Int

data Position = Position { horizontalPosition :: Int, depth :: Int } deriving (Show, Eq)

parseLine :: String -> Movement
parseLine (stripPrefix "down " -> Just intStr) = Down (read intStr :: Int)
parseLine (stripPrefix "up " -> Just intStr) = Up (read intStr :: Int)
parseLine (stripPrefix "forward " -> Just intStr) = Forward (read intStr :: Int)
parseLine _ = error "bad input"

moveOne :: Movement -> Position -> Position
moveOne (Down value) position@Position{ depth } = position { depth = depth + value }
moveOne (Up value) position@Position{ depth } = position { depth = depth - value }
moveOne (Forward value) position@Position{ horizontalPosition } = position { horizontalPosition = horizontalPosition + value }

moveAll :: [Movement] -> Position
moveAll = foldr moveOne Position { horizontalPosition = 0, depth = 0 }

main :: IO ()
main = do
    input <- getContents
    let movements = map parseLine (lines input)
    putStr $ show $ moveAll movements

