module Main where

import Data.List

main :: IO ()
main = print . maximum . filter isPandigital $ results [1..9999]

results :: [Int] -> [Int]
results = concatMap (flip conProducts [1..9])

isPandigital :: Int -> Bool
isPandigital = (['1'..'9'] ==) . sort . show
        
conProduct :: Int -> [Int] -> Int
conProduct n  = read . concatMap (show . (*n))

conProducts :: Int -> [Int] -> [Int]
conProducts n = map (conProduct n) . vars

vars :: [Int] -> [[Int]]
vars n = map (flip take n) n