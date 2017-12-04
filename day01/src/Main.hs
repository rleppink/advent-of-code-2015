module Main where

main :: IO ()
main = do
  rawInput <- readFile "input"

  print (foldr (\ a b -> if a == '(' then b + 1 else b - 1) 0 rawInput)
