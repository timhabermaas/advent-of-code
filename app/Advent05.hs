module Main where

import Data.List (isInfixOf, group)

isNiceString :: String -> Bool
isNiceString s = atLeastVowels 3 s &&
                   containsNot ["ab", "cd", "pq", "xy"] s &&
                   hasLetterWhichAppearsAtLeastTwice s
  where
    atLeastVowels n s = length (filter isVowel s) >= n
    isVowel c = c `elem` "aeiou"
    containsNot list s = not $ any (\seq -> isInfixOf seq s) list
    hasLetterWhichAppearsAtLeastTwice s = any ((> 1) . length) $ group s--any (\(x, y) -> x == y) $ zip s (tail s)

isNiceString2 :: String -> Bool
isNiceString2 s = atLeastOneLetterRepeatedWithOneGap s &&
                    pairOfLettersAppearsAtLeastTwice s
  where
    atLeastOneLetterRepeatedWithOneGap s = (>= 1) $ length $ filter (\(x, y) -> x == y) $ zip s (tail (tail s))
    pairOfLettersAppearsAtLeastTwice (x:y:xs) = isInfixOf [x,y] xs || pairOfLettersAppearsAtLeastTwice (y:xs)
    pairOfLettersAppearsAtLeastTwice _        = False

main :: IO ()
main = do
    content <- readFile "inputs/advent05.txt"
    putStrLn $ show $ length $ filter isNiceString $ lines content
    putStrLn $ show $ length $ filter isNiceString2 $ lines content
