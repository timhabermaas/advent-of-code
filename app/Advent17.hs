module Main where

import Data.List (subsequences)

main :: IO ()
main = do
    content <- readFile "inputs/advent17.txt"

    let values = fmap read $ lines content :: [Int]
    let permutations = subsequences values
    let goal = 150
    let solutions = filter (\p -> sum p == goal) permutations
    putStrLn $ show $ length $ solutions

    let minBucketCount = minimum $ fmap length solutions
    let solutionsWithMinBucketCount = filter ((== minBucketCount) . length) solutions
    putStrLn $ show $ length $ solutionsWithMinBucketCount
