import Data.Time.Clock
import Data.List (isPrefixOf, sortBy)

nums :: Int -> Int -> (Int, [Int])
nums n k = let nums' n k = let s = n * 10
                            in div s k : nums' (mod s k) k
            in (k, nums' n k)

cycle1 :: (Int, [Int]) -> Int -> (Int, [Int])
cycle1 (k, []) n    = (n, [])
cycle1 (k, l) n     = let cycle1' l' n' 
                            | n' > 1000 = []
                            | otherwise = let   t1 = take n' l'
                                                t2 = take n' $ drop n' l'
                                            in case (t1 == t2) of
                                                True    -> t1
                                                False   -> cycle1' l' (n' + 1)
                        in (k, cycle1' l n)

findCycle :: [Int] -> (Int, Int)
findCycle list = head $ 
                    sortBy (\(a, b) -> \(c, d) -> if b < d then GT else if b > d then LT else EQ) 
                        $ map (\(a, b) -> (a, length b)) 
                            $ map (\n -> cycle1 (nums 1 n) 50) list

main = do
    t1 <- getCurrentTime
    print $ findCycle [1..1000]
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1