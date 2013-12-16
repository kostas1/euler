module Main where
import System.TimeIt
import Math.NumberTheory.Primes
import Data.List

main :: IO ()
main = timeIt . print . head $ firstSetOf 4 4 [1..]
    
setsOf :: Integer -> [Integer] -> [[Integer]]
setsOf n = map (flip map [1..n] . (+))

firstSetOf :: Int -> Integer -> [Integer] -> [Integer]
firstSetOf n k = head . dropWhile (not . distinctOf n) . setsOf k

distinctOf :: Int -> [Integer] -> Bool
distinctOf n xs = all ((==n) . length) $ map factorise' xs