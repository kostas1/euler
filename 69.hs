module Main where
import System.TimeIt
import Data.Function
import Data.List
import Data.Ord
import Control.Monad
import Math.NumberTheory.Primes.Factorisation (totient)

main :: IO ()
main = timeIt . print . maximumBy (comparing snd) $ li
li = map (ap (,) nPhi) [1..1000000]
nPhi = liftM2 (/) fromIntegral (fromIntegral . totient)