import Data.Time.Clock
import Data.Bits
import Data.Char
import Data.List

keys :: [[Char]]
keys =  let l = ['a'..'z'] in
        [[a, b, c] | a <- l, b <- l, c <- l]



main = do
    t1 <- getCurrentTime
    a <- fmap (\a -> read a :: [Int]) (readFile "C:/Users/kostas/cipher1.txt")

--    [[Char]]
    let b = map (concat . repeat) keys

    let c = map (\l -> zipWith (\c -> \k -> xor c $ ord k) a l) b
    let l = (map (\b -> map chr b) c)
   -- writeFile "C:/Users/kostas/cipher2.txt" (unlines l)
    --print $ head $ drop 4423 keys
    --god ...
    print $ sum $ zipWith (\c -> \k -> xor c (ord k)) a $ (concat . repeat) "god"
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1
