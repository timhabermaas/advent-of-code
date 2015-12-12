module Main where

import Text.Parsec
import qualified Data.Map as Map
import Debug.Trace


type Coordinate = (Int, Int)
type Range = (Coordinate, Coordinate)

data Command = TurnOn | TurnOff | Toggle deriving Show
data Instruction = Instruction Command Range deriving Show

data LightGrid = LightGrid (Map.Map Coordinate Int)

initialLightGrid :: LightGrid
initialLightGrid = LightGrid Map.empty

toggle :: LightGrid -> Coordinate -> LightGrid
toggle l@(LightGrid map) c = LightGrid $ Map.insertWith (+) c 2 map

turnOn :: LightGrid -> Coordinate -> LightGrid
turnOn (LightGrid map) c = LightGrid $ Map.insertWith (+) c 1 map

turnOff :: LightGrid -> Coordinate -> LightGrid
turnOff (LightGrid map) c = LightGrid $ Map.update (\v -> if v - 1 == 0 then Nothing else Just $ v - 1) c map

litLights :: LightGrid -> Int
litLights (LightGrid map) = sum map

rangeToCoordinates :: Range -> [Coordinate]
rangeToCoordinates ((x0, y0), (x1, y1)) = [(x, y) | x <- [x0..x1], y <- [y0..y1]]


parseInstruction :: String -> Instruction
parseInstruction s = case parse instructionParser "" s of
    Left _ -> error $ "invalid input" ++ s
    Right x -> x
  where
    instructionParser = do
      command <- try turnOn <|> try turnOff <|> toggle
      char ' '
      range <- range
      return $ Instruction command range
    turnOn = TurnOn <$ string "turn on"
    turnOff = TurnOff <$ string "turn off"
    toggle = Toggle <$ string "toggle"
    range = do
      first <- pair
      string " through "
      second <- pair
      return (first, second)
    pair = do
      a <- many digit
      char ','
      b <- many digit
      return (read a, read b)

executeInstruction :: LightGrid -> Instruction -> LightGrid
executeInstruction l (Instruction command range) = foldl action l coordinates
  where
    action =
      case command of
        TurnOff -> turnOff
        Toggle -> toggle
        TurnOn -> turnOn
    coordinates = rangeToCoordinates range

runProgram :: [Instruction] -> LightGrid -> LightGrid
runProgram instructions l = foldl executeInstruction l instructions

main :: IO ()
main = do
    content <- readFile "inputs/Advent06.txt"
    let instructions = fmap parseInstruction $ lines content
    putStrLn $ show $ litLights $ runProgram instructions initialLightGrid
