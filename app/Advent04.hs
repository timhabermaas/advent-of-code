{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Digest.Pure.MD5 (md5, MD5Digest)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 (pack)

firstEq :: String -> MD5Digest -> Bool
firstEq s = (== s) . take (length s) . show

findHash :: String -> (MD5Digest -> Bool) -> [String] -> String
findHash secret p tails = fromJust $ find (\t -> p $ md5 $ pack $ secret ++ t) tails

main :: IO ()
main = do
  let secretKey = "bgvyzdsv"
  let stringNumbers = fmap show [1..]
  putStrLn $ findHash secretKey (firstEq "00000") stringNumbers
  putStrLn $ findHash secretKey (firstEq "000000") stringNumbers
