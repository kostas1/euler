module Main where
import System.TimeIt
import Data.Ratio

vals :: [Ratio Integer]
vals = map vals' [0..]
    where
        vals' 0 = 1 % 2
        vals' k = reduce (1 % 2) (vals !! (k - 1))
        
reduce a b = ((numerator a) * (denominator b)) % (((denominator a * denominator b) + numerator b))

main :: IO ()
main =
    timeIt . print $ length $ filter biggerNumerator $  map (+ (1 % 1)) $ take 1000 vals
    where
        numDigs = length . show
        biggerNumerator k = (numDigs $ numerator k) > (numDigs $ denominator k)