import Data.Time.Clock
import Data.List
import Math.NumberTheory.Primes.Sieve
import Math.NumberTheory.Primes

digits n = let d 0 = []; d i = mod i 10 : d (div i 10) in  d n

toNumber n = let t [] _ = 0; t (x:xs) l = x * (10 ^ l) + t xs (l + 1) in t n 0

circul l = let  c li 0 = [toNumber li]
                c (x:xs) n =    let ny = xs ++ [x] 
                                in toNumber (x:xs) : c ny (n - 1)
                lu = digits l
           in c lu (length lu - 1)

circular n = all isPrime $ circul n

main = do
    t1 <- getCurrentTime
    print $ length $ filter circular [1..1000000]
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1