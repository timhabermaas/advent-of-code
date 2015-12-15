{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Text.Regex.Posix

newtype Seconds = Seconds Int deriving (Show, Num, Eq, Ord, Real, Enum, Integral)
newtype KilometersPerSecond = KilometersPerSecond Int deriving (Show, Num, Eq, Ord, Real, Enum, Integral)
newtype Kilometers = Kilometers Int deriving (Show, Num, Eq, Ord, Real, Enum, Integral)

data Reindeer = Reindeer { name :: String,
                           speed :: KilometersPerSecond,
                           runTime :: Seconds,
                           restTime :: Seconds
                         } deriving (Eq, Show)

parseReindeer :: String -> Reindeer
parseReindeer s =
    let (_, _, _, [name, speed, runTime, restTime]) =
          s =~ "([a-zA-Z]+) can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds." :: (String, String, String, [String])
    in
      Reindeer name (KilometersPerSecond (read speed)) (Seconds (read runTime)) (Seconds (read restTime))

cycleDuration :: Reindeer -> Seconds
cycleDuration (Reindeer _ _ x y) = x + y

kilometersPerCycle :: Reindeer -> Kilometers
kilometersPerCycle (Reindeer _ (KilometersPerSecond speed) (Seconds runTime) _) = Kilometers $ speed * runTime

distanceTravelled :: KilometersPerSecond -> Seconds -> Kilometers
distanceTravelled (KilometersPerSecond kms) (Seconds s) = Kilometers $ kms * s

distanceAfterSeconds :: Seconds -> Reindeer -> Kilometers
distanceAfterSeconds totalTime r = Kilometers distance
  where
    (Seconds totalCycles) = totalTime `div` (cycleDuration r)
    restSeconds = totalTime - (Seconds totalCycles) * (cycleDuration r)
    distance = fullCycleDistance + restDistance
    (Kilometers fullCycleDistance) = kilometersPerCycle r * (Kilometers totalCycles)
    (Kilometers restDistance) = distanceTravelled (speed r) (min (runTime r) restSeconds)

incrementScores :: [(Reindeer, Int)] -> [Reindeer] -> [(Reindeer, Int)]
incrementScores scores reindeers = fmap (\(r, s) -> if r `elem` reindeers then (r, s + 1) else (r, s)) scores

scores :: [Reindeer] -> Seconds -> [(Reindeer, Int)]
scores reindeers totalTime = foldl (\scores seconds -> incrementScores scores (winners seconds)) (fmap (\r -> (r, 0)) reindeers) [1..totalTime]
  where
    winners seconds = fmap fst $ filter (\(r, d) -> d == winnerDistance seconds) $ resultAfter seconds
    winnerDistance seconds = maximum $ fmap snd $ resultAfter seconds
    resultAfter seconds = distances reindeers seconds
    distances reindeers seconds = fmap (\r -> (r, distanceAfterSeconds seconds r)) reindeers

main :: IO ()
main = do
    let time = Seconds 2503
    contents <- readFile "inputs/advent14.txt"
    let l = lines contents
    putStrLn $ show $ maximum $ fmap ((distanceAfterSeconds time) . parseReindeer) l
    putStrLn $ show $ maximum $ fmap snd $ scores (fmap parseReindeer l) time
