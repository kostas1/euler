module Main where

import Data.Time.Clock
import Data.List
import Data.Function

main :: IO ()
main = do
    t1 <- getCurrentTime
    print . sum . map head . nubBy ((==) `on` head) . filter weirdPanda $ vars
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1

weirdPanda :: [Int] -> Bool
weirdPanda = pandigital . toBig

pandigital :: Int -> Bool
pandigital = (==) ['1'..'9'] . sort . show

vars :: [[Int]]
vars = [[x * y, x, y] | x <- [1..1999], y <- [1..div x 2]]

toBig :: [Int] -> Int
toBig = read . concatMap show