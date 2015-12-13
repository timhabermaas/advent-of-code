module Main where

import Text.Parsec
import Data.List (nub, permutations)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

type Distance = Int
type City = String
data Path = Path City City Distance deriving Show


pathParser :: Parsec String () Path
pathParser = do
    fromName <- name
    string " to "
    toName <- name
    string " = "
    distance <- number
    return $ Path fromName toName distance
  where
    name = many1 letter
    number = read <$> many1 digit

runParser :: Parsec String () Path -> String -> Path
runParser p s =
    case parse p "" s of
      Left e -> error "parse error"
      Right x -> x

uniqCities :: [Path] -> [City]
uniqCities paths = nub $ concat $ fmap (\(Path a b _) -> [a, b]) paths

distanceMap :: [Path] -> Map.Map (City, City) Distance
distanceMap ((Path from to distance):ps) = Map.insert (to, from) distance $ Map.insert (from, to) distance $ distanceMap ps
distanceMap [] = Map.empty

totalDistance :: Map.Map (City, City) Distance -> [City] -> Distance
totalDistance distanceMap cities = sum $ fmap (\pair -> fromJust $ Map.lookup pair distanceMap) $ foo cities
  where
    foo (x:y:xs) = (x,y):(foo (y:xs))
    foo _ = []

main :: IO ()
main = do
    content <- readFile "inputs/advent09.txt"
    let paths = fmap (Main.runParser pathParser) $ lines content
    let distanceCatalog = distanceMap paths
    let distances = fmap (totalDistance distanceCatalog) $ permutations $ uniqCities paths
    putStrLn $ show $ minimum $ distances
    putStrLn $ show $ maximum $ distances
