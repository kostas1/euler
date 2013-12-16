module Main where
import System.TimeIt
import Math.NumberTheory.Primes
import Data.List

main :: IO ()
main = timeIt . print . concatMap show . head . tail . filter permtriplet $ variations


set = dropWhile (< 1000) . takeWhile (< 10000) $ primes

permutation a b = a /= b && (sort . show $ a) == (sort . show $ b)


variations :: [[Integer]]
variations = concatMap (\k -> map (\n -> triplet n k) . takeWhile (< (head set) + k) $ set) [1..5000]

triplet n k = [n, n + k, n + k + k]

permtriplet [a, b, c] =
    permutation a b &&
    permutation a c &&
    isPrime b && isPrime c