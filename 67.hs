module Main where
import System.TimeIt
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.List



file = readFile "C:/Users/kostas/Downloads/triangle.txt"
dataset' = fmap (\str -> map (\l -> map (\w -> read w :: Integer) $ words l) $ lines str) file



type IntegerTable = M.Map (Integer, Integer) (Integer, Integer)

main :: IO ()
main = do
    d <- dataset'
    let rows = [1..fromIntegral $ length d]
    timeIt . print $ maximumBy maxBy $ M.toList $  M.filterWithKey (\k v -> (fst k) == (fromIntegral $ length d)) $ calculate  (populate d) rows


maxBy :: Ord a => (a2, (a1, a)) -> (a2, (a1, a)) -> Ordering
maxBy = comparing (snd . snd)

populate :: [[Integer]] -> IntegerTable
populate = M.fromList . transform

transform :: [[Integer]] -> [((Integer, Integer), (Integer, Integer))]
transform = concatMap (\(r, cv) -> map (\(c, v) -> ((r, c), (v, 0))) cv) . zip [1..] . map (zip [1..])

calculate :: IntegerTable -> [Integer] -> IntegerTable
calculate m' rows =
    foldl (\m1 row ->
        foldl (\m2 column ->
            updateMap m2 row column
        ) m1 (columns row)
    ) m' rows



columns row = [1..row]

updateMap m r c = M.insert (r, c) (calculateRoute m r c) m

calculateRoute m 1 1 = (v, v)
    where
        (v, _) = m M.! (1, 1)
calculateRoute m r c = (v, v + (max tl tr))
    where
        (v, _) = m M.! (r, c)
        tl = topLeft m r c
        tr = topRight m r c

topLeft m r c =   snd $ fromMaybe (0, 0) (M.lookup (r - 1, c - 1) m)
topRight m r c =  snd $ fromMaybe (0, 0) (M.lookup (r - 1, c) m)
