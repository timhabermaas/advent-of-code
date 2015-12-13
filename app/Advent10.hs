module Main where

counter :: Eq a => [a] -> [(Int, a)]
counter (s:xs) = counter' s 1 xs
  where
    counter' last count (s:xs) =
      if last == s then
        counter' last (count + 1) xs
      else
        (count, last):counter' s 1 xs
    counter' last count [] = [(count, last)]

magic :: String -> String
magic s = concat $ fmap (\(count, char) -> show count ++ [char]) $ counter s

applyNTimes :: (a -> a) -> Int -> a -> a
applyNTimes f n x = last $ take (n + 1) $ iterate f x

main :: IO ()
main = do
    let input = "1113122113" :: String
    putStrLn $ show $ length $ applyNTimes magic 40 input
    putStrLn $ show $ length $ applyNTimes magic 50 input
