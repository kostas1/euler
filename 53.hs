import Data.Time.Clock

c n r = round $ (fac n) / ((fac r) * (fac (n - r)))

re = concat $ map (\n -> map (\k -> c n k) [1..n]) [1..100]

main = do
    t1 <- getCurrentTime
    print $ length $ filter (>1000000) re
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1
