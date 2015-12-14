module Main where

import Text.Parsec
import Data.List (nub, permutations, find)

type Happiness = Int
type Name = String
data Rule = Rule (Name, Name) Happiness deriving Show

rulesParser :: Parsec String () Rule
rulesParser = do
    n1 <- name
    string " would "
    factor <- (id <$ string "gain ") <|> ((* (-1)) <$ string "lose ")
    happiness <- factor . read <$> number
    string " happiness units by sitting next to "
    n2 <- name
    char '.'
    return $ Rule (n1, n2) happiness
  where
    name = many1 letter
    number = many1 digit

runParser :: Parsec String () Rule -> String -> Rule
runParser p s =
    case parse p "" s of
      Left e -> error "parse error"
      Right x -> x

uniqNames :: [Rule] -> [Name]
uniqNames ((Rule (a, b) _):rs) = nub $ [a, b] ++ uniqNames rs
uniqNames [] = []

neighbours :: [a] -> [(a, a)]
neighbours (n:ns) = neighbours' n (n:ns)
  where
    neighbours' fst (x:y:ns) = (x, y):(y, x):neighbours' fst (y:ns)
    neighbours' fst [x]      = [(x, fst), (fst, x)]
    neighbours' fst []       = []

happiness :: [Rule] -> (Name, Name) -> Int
happiness rules names =
    case find (\(Rule p _) -> p == names) rules of
      Just (Rule _ n) -> n

rankSeating :: [Rule] -> [Name] -> Int
rankSeating rules names = foldl (\sum n -> sum + happiness rules n) 0 $ neighbours names

bestSeating :: [Rule] -> Int
bestSeating rules = maximum $ fmap (rankSeating rules) possibleSeatings
  where
    names = uniqNames rules
    possibleSeatings = permutations names

main :: IO ()
main = do
    content <- readFile "inputs/advent13.txt"
    let rules = fmap (Main.runParser rulesParser) $ lines content

    let names = uniqNames rules
    let rulesWithMe = rules ++ (concat $ fmap (\name -> [Rule (name, "Tim") 0, Rule ("Tim", name) 0]) names)

    putStrLn $ show $ bestSeating rules
    putStrLn $ show $ bestSeating rulesWithMe
