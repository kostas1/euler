import Data.Time.Clock
import Data.List

toNumber :: [Integer] -> Integer
toNumber li =   let to [] _ = 0
                    to (x:xs) n = x * (10 ^ n) + to xs (n + 1) 
                in to (reverse li) 0

pandigitals :: [[Integer]]
pandigitals = filter (\a -> head a /= 0) $ permutations [0..9]

indexes :: [[Int]]
indexes = zipWith (\a -> \b -> [a..b]) [1..7] [3..9]

subnumbers :: [Integer] -> [Integer]
subnumbers li = map (\i -> toNumber $ map (\o -> li !! o) i) indexes

hasProperty :: [Integer] -> Bool
hasProperty li = all (==True) $ map (\(d, n) -> mod d n == 0) $ zip li [2, 3, 5, 7, 11, 13, 17]

main = do
    t1 <- getCurrentTime
    print $ sum $ map toNumber $ filter (\a -> hasProperty $ subnumbers a) pandigitals
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1
