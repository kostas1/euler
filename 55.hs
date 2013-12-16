import Data.Time.Clock

isLychrel :: Integer -> Int -> Bool
isLychrel _ 50  = True
isLychrel i n   =   let next = i + (reverse' i) in
                    case pal next of
                        True    -> False
                        False   -> isLychrel next (n + 1)
                    where   digits 0 = []
                            digits k = mod k 10 : digits (div k 10)
                            toNumber [] l       = 0
                            toNumber (x:xs) l   = x * (10 ^ l) + (toNumber xs (l + 1))
                            reverse' k = toNumber (reverse $ digits k) 0
                            pal k = (digits k) == (reverse $ digits k)

main = do
    t1 <- getCurrentTime
    print $ length $ filter (\n -> isLychrel n 1) [1..10000]
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1