module Main where
import System.TimeIt
import Data.Function (on)

main :: IO ()
main = timeIt . print $ length $ filter hcfOne' $ filter vsubset vars

hcfOne' (a, b) = (gcd a b) == 1

vars = [(n, d) | d <- [1..12000], n <- [1 + div d 3..div d 2]]

division = uncurry ((/) `on` fromIntegral)

vsubset c@(a, b) = c' > division (1, 3) && c' < division (1, 2)
    where
        c' = division c
