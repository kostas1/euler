import Math.NumberTheory.Primes.Sieve
import Data.List
import Math.NumberTheory.Primes

vars :: Integer -> [Integer]
vars 0 = []
vars n = n : vars (div n 10)

vars' :: Integer -> Integer -> [Integer]
vars' n k = case div n (10 ^ k) == 0 of
                True -> n : []
                False -> mod n (10 ^ k) : vars' n (k + 1)

allVars :: Integer -> [Integer]
allVars n = nub $ vars n ++ (vars' n 1)

truncatable :: Integer -> Bool
truncatable n = all isPrime $ allVars n

main = do
    t1 <- getCurrentTime
    print $ sum $ take 11 $ filter truncatable $ filter (> 10) primes
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1