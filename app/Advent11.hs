module Main where

import Data.Char (ord)
import Data.List (isInfixOf)

succString :: String -> String
succString input = reverse $ succString' $ reverse input
  where
    succString' (x:xs) =
      if succ x == '{' then
        'a':succString' xs
      else
        (succ x):xs
    succString' [] = []


isValidPassword :: String -> Bool
isValidPassword password =
    increasingStraightOfThreeCharacters password
      && withoutCharacters "iol" password
      && pairOfLettersAppearsAtLeastTwice password
  where
    increasingStraightOfThreeCharacters password = not . null $ filter isIncreasingTriplet $ triplets password
    withoutCharacters cs password = all (not . (flip elem $ password)) cs
    isIncreasingTriplet (a, b, c) = ord a == (ord b) - 1 && ord b == (ord c) - 1
    triplets (x:y:z:xs) = (x, y, z):triplets (y:z:xs)
    triplets _ = []

    pairOfLettersAppearsAtLeastTwice xs = pairCounter xs >= 2
    pairCounter (x:y:xs)
      | x == y    = succ $ pairCounter xs
      | otherwise = pairCounter (y:xs)
    pairCounter _ = 0

santasNewPassword :: String -> String
santasNewPassword oldPassword = head $ filter isValidPassword $ tail $ iterate succString oldPassword

main :: IO ()
main = do
    let oldPassword = "hepxcrrq"
    let newPassword = santasNewPassword oldPassword
    putStrLn $ newPassword
    putStrLn $ santasNewPassword newPassword
