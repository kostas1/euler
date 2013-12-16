import Data.Time.Clock
import Data.List

digits :: Int -> [Int]
digits n' = let dig 0 = []; dig n = mod n 10 : dig (div n 10) in dig n'

next :: Int -> Int
next n = sum $ map (^2) $ digits n

chain' :: Int -> Int
chain' n = let c 1 _ = 1; c 89 _ = 89; c k l = c l (next l) in c n n

main = do
    t1 <- getCurrentTime
    print $ length $ filter (==89) $ map chain' [1..10000000]
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1