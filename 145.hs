import Data.Time.Clock
import Data.Bits

digits :: Int -> [Int]
digits 0 = []
digits n = mod n 10 : digits (div n 10)

toNumber n = toNumber' n 0
toNumber' [] _ = 0
toNumber' l n = (head l) * (round (10**n)) + (toNumber' (tail l) (n + 1))

allOdd n = all (\a -> mod a 2 == 1) n

reversible' n = let rev = digits n
                in case (head rev) of
                    0 -> False
                    _ -> allOdd $ digits $ n + (toNumber $ reverse $ rev)

main = do
    t1 <- getCurrentTime
   -- print $ length $ reverses 100000
    print $ length $ filter reversible' [1..1000000000]
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1
