import Data.Char
import System.Time
import Data.List

main = do
    t1 <- getClockTime
    print $ iter (\a -> let str = show a
                        in case ((length $ take 18 str)==18) of
                        True -> (pandigital $ take 9 str) && (pandigital $ show $ (mod a 1000000000))
                        False -> False) 1 1 1
    t2 <- getClockTime
    print $ diffClockTimes t1 t2

iter :: Num a => (a -> Bool) -> Int -> a -> a -> Int
iter f n a b = case (f a) of
    True -> n
    False -> iter f (n+1) b (a + b)

pandigital :: [Char] -> Bool
pandigital str
    | length str /= (length $ nub str) = False
    | otherwise = all (\n -> isDigit n && n /= '0') (nub str)