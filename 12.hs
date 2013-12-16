import Data.Time.Clock
import Data.List
import Math.NumberTheory.Primes.Factorisation (divisors)
import Data.Set as DS (size)

main = do
    t1 <- getCurrentTime
    print $ head $ dropWhile (\(c, d) -> c < 500) $ map (\a -> let n = divisors a in (DS.size n, a)) (scanl1 (+) [1..])
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1