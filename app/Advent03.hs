module Main where

import Data.List (nub)

type Vec2 = (Int, Int)

data Direction =
    North
    | South
    | West
    | East
    deriving Show

listOfDirections :: String -> [Direction]
listOfDirections = fmap directionForChar
  where
    directionForChar '<' = West
    directionForChar '^' = North
    directionForChar 'v' = South
    directionForChar '>' = East

positions :: [Direction] -> [Vec2]
positions = foldl (\acc d -> (move d $ head acc) : acc) [(0,0)]
  where
    move d (x, y) = case d of
                      West  -> (x - 1, y)
                      North -> (x, y + 1)
                      South -> (x, y - 1)
                      East  -> (x + 1, y)

oddElements :: [a] -> [a]
oddElements (x:y:xs) = x : oddElements xs
oddElements xs = xs

evenElements :: [a] -> [a]
evenElements (_:y:xs) = y : evenElements xs
evenElements [x] = []
evenElements [] = []

santaPositions :: [Direction] -> [Vec2]
santaPositions = positions . oddElements

robotPositions :: [Direction] -> [Vec2]
robotPositions = positions . evenElements

main :: IO ()
main = do
    content <- readFile "inputs/advent03.txt"
    let directions = listOfDirections content

    let houses1 = positions directions
    putStrLn $ show $ length $ nub houses1

    let houses2 = santaPositions directions ++ robotPositions directions
    putStrLn $ show $ length $ nub houses2
