module Main where
import System.TimeIt
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.List
import Math.NumberTheory.Primes
import Debug.Trace


main :: IO ()
--main = timeIt . print $ take 5 $ map (take 8) primeSums
main = timeIt . print $ maximumBy (comparing (snd . snd)) $ takeWhile ((<1000000) . fst) $ dropWhile ((<990000) . fst) primesWithLengths
--main = timeIt . print $ sequenceLength 92951

primeSums :: [[Integer]]
primeSums = map (tail . scanl (+) 0) . tails $ primes

primesWithLengths :: [(Integer, (Integer, Maybe Int))]
primesWithLengths = zip primes (map sequenceLength primes)

sequenceLength :: Integer -> (Integer, Maybe Int)
sequenceLength p = seq p primeSums 0
    where
        seq num (x:xs) n
            | head x > div num 5000 = (0, Nothing)
            | otherwise     = case index of
                Just i  -> (n, Just (i + 1))
                Nothing -> seq num xs (n + 1)
              where
                index = elemIndex num $ takeWhile (< k) x
                k = num + 1