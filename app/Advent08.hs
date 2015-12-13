module Main where

decode :: String -> String
decode s = removeEscapes (init (tail s))
  where
    removeEscapes ('\\':'\\':xs) = '\\':(removeEscapes xs)
    removeEscapes ('\\':'"':xs) = '"':(removeEscapes xs)
    removeEscapes ('\\':'x':x:y:xs) = '?':(removeEscapes xs)
    removeEscapes (x:xs) = x:(removeEscapes xs)
    removeEscapes [] = []

encode :: String -> String
encode s = "\"" ++ (escapeCharacters s) ++ "\""
  where
    escapeCharacters ('\\':xs) = "\\\\" ++ escapeCharacters xs
    escapeCharacters ('"':xs) = "\\\"" ++ escapeCharacters xs
    escapeCharacters (x:xs) = x:(escapeCharacters xs)
    escapeCharacters [] = []



main :: IO ()
main = do
  content <- readFile "inputs/advent08.txt"
  let cleanedCounts = sum $ fmap (length . decode) $ lines content
  let sourceCounts = sum $ fmap length $ lines content
  putStrLn $ show $ sourceCounts - cleanedCounts
  let encodedStringCharacters = sum $ fmap (length . encode) $ lines content
  putStrLn $ show $ encodedStringCharacters - sourceCounts
