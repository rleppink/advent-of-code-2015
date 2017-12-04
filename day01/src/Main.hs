module Main where

import Data.List


main :: IO ()
main = do
  rawInput <- readFile "input"

  -- Day 1.1
  print
    (foldr
      (\ a b ->
         if a == '(' then b + 1 else b - 1)
      0
      rawInput)

  -- Day 1.2
  print
    (elemIndex (-1) $
     scanl
       (\ b a ->
          if a == '(' then b + 1 else b - 1)
       0
       rawInput)
