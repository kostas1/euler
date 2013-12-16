module Main where
import System.TimeIt
import Math.NumberTheory.Primes
import Data.List
import Data.Function

main :: IO ()
main = timeIt . print $ set !! 100000

set :: [[Integer]]
set = map set' [0..]
    where
        set' 0 = [1]
        set' 1 = map ((+ k) . (*) (1 * 2)) [1..4]
            where
                k = head $ set' 0
        set' n = map ((+ k) . (*) (n * 2)) [1..4]
            where
                k = last $ set' (n - 1)

r :: (Fractional a, Ord a) => a -> a -> a -> Bool
r k a b = k <= (a / b)

-- primes, all, step
ratios :: [(Int, Int, Int)]
ratios = map ratios' $ zip (zip set [1,3..]) [0..]
    where
        ratios' :: (([Integer], Int), Int) -> (Int, Int, Int)
        ratios' (_, 0) = (0, 1, 1)
        ratios' a@(_, b) = glue a (ratios !! (b - 1))
        glue ((a, b), c) (d, e, f) = (d + pr a, e + (length a), b)
        pr = length . filter isPrime

first' :: (Fractional a, Num a, Ord a) => a -> (Int, Int, Int)
first' n = head . dropWhile (\(a, b, _) -> r n (fromIntegral a) (fromIntegral b)) $ tail ratios