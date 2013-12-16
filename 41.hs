import Data.Time.Clock
import Data.List
import Math.NumberTheory.Primes.Sieve
import Math.NumberTheory.Primes

pandigital :: Integer -> Bool
pandigital n = let  digits 0 = []
                    digits n = mod n 10 : digits (div n 10)
                    li = digits n
                    le = fromIntegral $ length li
                    lu n = length n == 0
               in lu $ dropWhile (\(a, b) -> a == b) $ zip [1..le] (sort li)

main = do
    t1 <- getCurrentTime
    print $ maximum $ filter pandigital $ takeWhile (< 1000000000) primes
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1