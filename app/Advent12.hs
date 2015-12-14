{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as Map
import Data.Vector (toList)
import Data.Scientific (toBoundedInteger)
import qualified Data.ByteString.Lazy.Char8 as BS

import Debug.Trace

findNumbers :: Value -> [Int]
findNumbers (Object o) = concat $ fmap findNumbers $ Map.elems o
findNumbers (Array a) = concat $ fmap findNumbers $ toList a
findNumbers (String s) = []
findNumbers (Number n) = [fromJust $ toBoundedInteger n]
findNumbers (Null) = []

findNumbers' :: Value -> [Int]
findNumbers' (Object o)
    | "red" `elem` Map.elems o = []
    | otherwise = concat $ fmap findNumbers' $ Map.elems o
findNumbers' (Array a) = concat $ fmap findNumbers' $ toList a
findNumbers' (String s) = []
findNumbers' (Number n) = [fromJust $ toBoundedInteger n]
findNumbers' (Null) = []

main :: IO ()
main = do
    content <- BS.readFile "inputs/advent12.txt"
    let (Just v) = decode content :: Maybe Value
    putStrLn $ show $ sum $ findNumbers v
    putStrLn $ show $ sum $ findNumbers' v
