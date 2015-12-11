module Main where

import Text.Parsec

type Dimensions = (Int, Int, Int)

requiredWrappingPaper :: Dimensions -> Int
requiredWrappingPaper (l, w, h) = (sum sides) * 2 + extraPaper
  where
    sides = [l * w, w * h, l * h]
    extraPaper = minimum sides

ribbonLength :: Dimensions -> Int
ribbonLength d = presentVolume d + smallestPerimeter d
  where
    smallestPerimeter (l, w, h) = 2 * (minimum [l + w, w + h, h + l])
    presentVolume (l, w, h) = l * w * h

parseDimensions :: String -> Dimensions
parseDimensions s = case parse dimensionParser "" s of
    Left _ -> error "nope"
    Right x -> x
  where
    dimensionParser = do
      a <- many digit
      char 'x'
      b <- many digit
      char 'x'
      c <- many digit
      return (read a, read b, read c)

main :: IO ()
main = do
    content <- readFile "inputs/advent02.txt"

    let dimensions = fmap parseDimensions $ lines content
    let squareFeets = fmap requiredWrappingPaper dimensions
    let ribbonLenghts = fmap ribbonLength dimensions

    putStrLn $ show $ (sum squareFeets, sum ribbonLenghts)
