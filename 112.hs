import System.Time
import Data.List

-- http://projecteuler.net/problem=112
-- takes approximately 1.3s to find a solution
main = do
    t1 <- getClockTime
    print $ iter    (\(a,b,c) -> c == 0.99) 
                    (\n (a,b,c) -> case (isBouncy n) of
                                    True -> (a+1, b+1, (fromIntegral $ b+1) / (fromIntegral $ a+1))
                                    False -> (a+1, b, (fromIntegral b) / (fromIntegral $ a+1)))
                    (0,0,0.0)
                    [1..]                                                        
    t2 <- getClockTime
    print $ diffClockTimes t1 t2

iter :: (b -> Bool) -> (a -> b -> b) -> b -> [a] -> b
iter _ _ def [] = def
iter bo f d (x:xs) = 
    let nex = f x d
    in case (bo nex) of
        True -> nex
        False -> iter bo f nex xs

isBouncy :: Integer -> Bool
isBouncy ll = do
    let l = toDigits ll
    (not $ isIncreasing l) && (not $ isIncreasing $ reverse l)

isIncreasing :: [Integer] -> Bool
isIncreasing l = do
    let (a, b) = foldl' (\(a, bo) b -> (b, bo && (b >= a))) (0, True) l
    b

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = (mod n 10) : toDigits (div n 10)