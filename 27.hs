module Main where
import System.TimeIt
import Math.NumberTheory.Primes
import Data.List
import Data.Function
import Data.Ord

main :: IO ()
main = timeIt . print . maximumBy (comparing snd) $ vals

formulas :: [Integer -> Integer]
formulas = map buildFormula combs

buildFormula :: (Integer, Integer) -> (Integer -> Integer)
buildFormula (a, b) = \n -> n * n + a * n + b

combs :: [(Integer, Integer)]
combs = [(a, b) | a <- [-999, -998..999], b <- [-999, -998..999]]

consPrimes :: (Integer -> Integer) -> Int
consPrimes f = length . takeWhile isPrime . map f $ [0..]

vals :: [((Integer, Integer), Int)]
vals = zip combs (map (consPrimes . buildFormula) combs)