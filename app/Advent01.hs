module Main where

import Data.List (findIndex)
import Data.Maybe (fromJust)

bracketToNumber :: Char -> Int
bracketToNumber '(' = 1
bracketToNumber ')' = -1
bracketToNumber _ = 0

floors :: String -> [Int]
floors = reverse . snd . foldl (\(sum, acc) s -> (sum + bracketToNumber s, (sum + bracketToNumber s) : acc)) (0, [])

firstBasementPosition :: String -> Maybe Int
firstBasementPosition = findIndex (< 0) . floors

lastFloor :: String -> Int
lastFloor = last . floors

main :: IO ()
main = do
  input <- readFile "inputs/advent01.txt"
  putStrLn $ show $ lastFloor input
  putStrLn $ show $ (fromJust $ firstBasementPosition input) + 1
