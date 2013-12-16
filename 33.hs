module Main where

import Data.List

variations :: [(Int, Int)]
variations = filtered vars
    where   filtered = filter (not . trivial) . filter hasSameDigits . filter (\(x, y) -> div x y < 1)
            vars = [(x, y) | x <- [10..99], y <- [10..99]]
            mapped = map (\(x, y) -> (digits x, digits y))
            
digits :: Int -> [Int]
digits x = digits' x []

digits' :: Int -> [Int] -> [Int]
digits' 0 [] = [0]
digits' 0 n = n
digits' x n = digits' (div x 10) ((mod x 10) : n)

trivial :: (Int, Int) -> Bool
trivial (a, b) = mod a 10 == 0 && mod b 10 == 0

hasSameDigits :: (Int, Int) -> Bool
hasSameDigits (a, b) = any (flip elem digsB) digsA
                    || any (flip elem digsA) digsB
    where
        digsA = digits a
        digsB = digits b
        
reset :: (Int, Int) -> (Int, Int)
reset (a, b) = comp (digsA, digsB)
    where
        comp ([c, d], [e, f]) = case (c == e, c == f, d == e, d == f) of
            (True, _, _, _) -> (d, f)
            (_, True, _, _) -> (d, e)
            (_, _, True, _) -> (c, f)
            (_, _, _, True) -> (c, e)
        digsB = digits b
        digsA = digits a
        
isCurious :: (Int, Int) -> Bool
isCurious (a, b) = divided (a, b) == r
    where
        r = divided $ reset (a, b)
        
divided (a, b) = (fromIntegral a) / (fromIntegral b)

curious :: [(Int, Int)]
curious = filter isCurious variations

result :: (Int, Int)
result = foldl (\(a, b) (c, d) -> (a * c, b * d)) (1, 1) curious

main :: IO ()
main = print $ divided (snd result, fst result)
