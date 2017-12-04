module Main where

import Data.List
import Data.List.Split


main :: IO ()
main = do
  rawInput <- readFile "input"
  let input = lines rawInput

  print (sum (map (wrappingPaperNeeded . stringToBox) input))
  print (sum (map (ribbonNeeded . stringToBox) input))


data Box =
  Box
    { boxLength :: Int
    , boxWidth  :: Int
    , boxHeight :: Int
    } deriving Show


stringToBox :: String -> Box
stringToBox x =
  Box
    (read $ stringSplit !! 0)
    (read $ stringSplit !! 1)
    (read $ stringSplit !! 2)
  where stringSplit = splitOn "x" x

lengthWidth :: Box -> Int
lengthWidth x = boxLength x * boxWidth x

widthHeight :: Box -> Int
widthHeight x = boxWidth  x * boxHeight x

heightLength :: Box -> Int
heightLength x = boxHeight x * boxLength x

totalArea :: Box -> Int
totalArea x = 2 * lengthWidth x + 2 * widthHeight x + 2 * heightLength x

smallestArea :: Box -> Int
smallestArea x = minimum [lengthWidth x, widthHeight x, heightLength x]

wrappingPaperNeeded :: Box -> Int
wrappingPaperNeeded x = totalArea x + smallestArea x

smallestPerimeter :: Box -> Int
smallestPerimeter x =
  minimum [ boxWidth x * 2 + boxLength x * 2
          , boxLength x * 2 + boxHeight x * 2
          , boxHeight x * 2 + boxWidth x * 2
          ]

ribbonNeeded :: Box -> Int
ribbonNeeded x =
  smallestPerimeter x + (boxWidth x * boxLength x * boxHeight x)
