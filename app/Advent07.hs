module Main where

import Data.Maybe (fromJust)
import Text.Parsec
import Text.ParserCombinators.Parsec.Error
import qualified Data.Map.Strict as Map
import Data.Word (Word16)
import Data.Bits
import qualified Data.MemoUgly as Memo

type Variable = String
type Output = Variable

data Wire = Wire Gate Output deriving Show
data Input  = Var Variable | Num Word16 deriving Show
data Gate   = Or Input Input
            | And Input Input
            | LShift Input Input
            | RShift Input Input
            | Not Input
            | Pure Input
            deriving Show

createWiring :: [Wire] -> Wiring
createWiring ws = Wiring $ foldl (\map w@(Wire _ o) -> Map.insert o w map) Map.empty ws

data Wiring = Wiring (Map.Map Variable Wire) deriving Show


computeValue :: Wiring -> Variable -> Word16
computeValue w@(Wiring map) v =
    innerComputeValue v
  where
    memoizedF = Memo.memo innerComputeValue
    innerComputeValue v =
      case fromJust $ Map.lookup v map of
        (Wire (Pure x) _) -> computePrimitive x
        (Wire (Not x) _) -> complement $ computePrimitive x
        (Wire (And x y) _) -> computePrimitive x .&. computePrimitive y
        (Wire (Or x y) _) -> computePrimitive x .|. computePrimitive y
        (Wire (LShift x y) _) -> shift (computePrimitive x) (fromIntegral $ computePrimitive y)
        (Wire (RShift x y) _) -> shift (computePrimitive x) ((* (-1)) $ fromIntegral $ computePrimitive y)
    computePrimitive (Var x) = memoizedF x
    computePrimitive (Num n) = n

setWireTo :: Wiring -> Variable -> Word16 -> Wiring
setWireTo w@(Wiring map) v x = Wiring $ Map.update (\_ -> Just $ Wire (Pure (Num x)) v) v map

parseWire :: String -> Wire
parseWire s =
    case parse wireParser "" s of
      Left e -> error $ show $ fmap messageString $ errorMessages e
      Right x -> x
  where
    wireParser = do
      gate <- lhs
      string " -> "
      output <- rhs
      return $ Wire gate output
    lhs = do
      try not <|> try or <|> try and <|> try rshift <|> try lshift <|> pure
    or = binaryOperation "OR" Or
    and = binaryOperation "AND" And
    rshift = binaryOperation "RSHIFT" RShift
    lshift = binaryOperation "LSHIFT" LShift
    binaryOperation keyword constructor = do
      i1 <- input
      string $ " " ++ keyword ++ " "
      i2 <- input
      return $ constructor i1 i2
    pure = do
      i <- input
      return $ Pure i
    not = do
      string "NOT "
      i <- input
      return $ Not i
    input = do
          Var <$> variable
      <|> Num <$> number
    variable = many1 $ oneOf ['a'..'z']
    number = read <$> many1 digit
    rhs = variable

main :: IO ()
main = do
    content <- readFile "inputs/advent07.txt"
    let l = lines content
    let wires = fmap parseWire l
    let wiring = createWiring wires
    let valueAtWireA = computeValue wiring "a"
    putStrLn $ show $ valueAtWireA
    let newWiring = setWireTo wiring "b" valueAtWireA
    let newValueAtWireA = computeValue newWiring "a"
    putStrLn $ show newValueAtWireA
