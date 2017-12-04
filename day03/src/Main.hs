module Main where

import Data.List


main :: IO ()
main = do
  input <- readFile "input"

  -- Day 3.1
  print (length $ nub $ scanl move (Position 0 0) input)

  -- Day 3.2
  let santaList = scanl move (Position 0 0) (movesFromInput even input)
  let roboList  = scanl move (Position 0 0) (movesFromInput odd  input)

  print (length $ nub $ union santaList roboList)


data Position =
  Position
    { xPos :: Int
    , yPos :: Int
    } deriving (Show, Eq)


movesFromInput :: (Int -> Bool) -> String -> String
movesFromInput p x = map snd (filter (p . fst) $ zip [0..] x)

move :: Position -> Char -> Position
move x y =
  case y of
    '^' -> Position (xPos x)     (yPos x + 1)
    '>' -> Position (xPos x + 1) (yPos x)
    '<' -> Position (xPos x - 1) (yPos x)
    'v' -> Position (xPos x)     (yPos x - 1)
    _   -> x

