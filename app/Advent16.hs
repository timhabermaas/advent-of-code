module Main where

import qualified Data.Map as Map
import Text.Parsec
import Debug.Trace

data Aunt = Aunt Int (Map.Map String Int) deriving Show
type PredicateMap = Map.Map String (Int -> Bool)

auntParser :: Parsec String () Aunt
auntParser = do
    string "Sue "
    n <- many1 digit
    string ": "
    pairs <- many1 pair
    return $ Aunt (read n) $ Map.fromList pairs
  where
    pair :: Parsec String () (String, Int)
    pair = do
      name <- many1 letter
      string ": "
      count <- many1 digit
      optional (string ", ")
      return (name, read count)


readAunt :: String -> Aunt
readAunt s =
    case runParser auntParser () "" s of
      Left e -> error $ show e
      Right r -> r

hasAttributes :: PredicateMap -> Aunt -> Bool
hasAttributes map (Aunt _ auntMap) = and $ Map.elems $ Map.intersectionWith (\f b -> f b) map auntMap

filterAunts :: PredicateMap -> [Aunt] -> [Aunt]
filterAunts map = (filter . hasAttributes) map

mfcsam1 :: PredicateMap
mfcsam1 = Map.fromList [("children", (== 3)),
                        ("cats", (== 7)),
                        ("samoyeds", (== 2)),
                        ("pomeranians", (== 3)),
                        ("akitas", (== 0)),
                        ("vizslas", (== 0)),
                        ("goldfish", (== 5)),
                        ("trees", (== 3)),
                        ("cars", (== 2)),
                        ("perfumes", (== 1))]

mfcsam2 :: PredicateMap
mfcsam2 = Map.fromList [("children", (== 3)),
                        ("cats", (> 7)),
                        ("samoyeds", (== 2)),
                        ("pomeranians", (< 3)),
                        ("akitas", (== 0)),
                        ("vizslas", (== 0)),
                        ("goldfish", (< 5)),
                        ("trees", (> 3)),
                        ("cars", (== 2)),
                        ("perfumes", (== 1))]

main :: IO ()
main = do
  content <- readFile "inputs/Advent16.txt"
  putStrLn $ show $ filterAunts mfcsam1 $ fmap readAunt $ lines content
  putStrLn $ show $ filterAunts mfcsam2 $ fmap readAunt $ lines content
