import Data.Time.Clock
import Data.List
import Math.NumberTheory.Primes.Sieve
import Math.NumberTheory.Primes
import Math.NumberTheory.Powers

nearestPrime :: Integer -> Integer
nearestPrime n = head $ dropWhile (not . isPrime) [n - 1, n - 2..]

oddComposite :: Integer -> Bool
oddComposite n =    let check p = isKthPower 2 (div (n - p) 2)
                        comp p
                            | p == 2    = False
                            | otherwise = case check p of
                                True    -> True
                                False   ->  comp $ nearestPrime p
                    in comp $ nearestPrime n

main = do
    t1 <- getCurrentTime
    print $ head $ filter (not . oddComposite) $ filter (not . isPrime) [17, 19..]
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1