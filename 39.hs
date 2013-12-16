module Main where

import Data.List

main :: IO ()
main = print $ head $ maximumBy len $ map solutions [1..1000] 
    where
        len = \a b -> if length a > length b then GT else LT

solutions :: Int -> [[Int]]
solutions p = 
    noob $ pitagorian $ filter ((== p) . head) combs
    where
        noob = nubBy (\[a, b, c, d] [a', b', c', d'] -> c == d' || d == c')
        pitagorian = filter (\[a, b, c, d] -> b^2 == c^2 + d^2)
        combs = [[x + y + z, x, y, z] | x <- [1..(div p 2)], y <- [1..x], z <- [1..x]]