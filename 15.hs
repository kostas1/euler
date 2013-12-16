import Data.Time.Clock
import Data.List
import Math.NumberTheory.Primes.Sieve
import Math.NumberTheory.Primes

fac :: Integer -> Integer
fac n = product [1..n]

coeff :: Integer -> Integer
coeff n = div (fac (2 * n)) ((fac n)^2)

-- below written function revealed that numbers are central binomial coefficients.
-- that let forming above function

routes :: Integer -> Integer -> Integer
routes cols rows = let  routes' c r =
                            let goRight = if c == cols then 0 else routes' (c + 1) r
                                goDown  = if r == rows then 0 else routes' c (r + 1)
                            in case (c == cols && r == rows) of
                                True    -> 1
                                False   -> goRight + goDown
                   in routes' 0 0
                   
main = do
    t1 <- getCurrentTime
    print $ coeff 20
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1
