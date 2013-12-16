module Main where
import System.TimeIt
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.List

dataset =
    [[75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20, 04, 82, 47, 65],
    [19, 01, 23, 75, 03, 34],
    [88, 02, 77, 73, 07, 63, 67],
    [99, 65, 04, 28, 06, 16, 70, 92],
    [41, 41, 26, 56, 83, 40, 80, 70, 33],
    [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

type IntegerTable = M.Map (Integer, Integer) (Integer, Integer)

main :: IO ()
main = timeIt . print $ maximumBy maxBy $ M.toList $  M.filterWithKey (\k v -> (fst k) == (fromIntegral $ length dataset)) $ calculate  (populate dataset)


maxBy :: Ord a => (a2, (a1, a)) -> (a2, (a1, a)) -> Ordering
maxBy = comparing (snd . snd)

populate :: [[Integer]] -> IntegerTable
populate = M.fromList . transform

transform :: [[Integer]] -> [((Integer, Integer), (Integer, Integer))]
transform = concatMap (\(r, cv) -> map (\(c, v) -> ((r, c), (v, 0))) cv) . zip [1..] . map (zip [1..])

calculate :: IntegerTable -> IntegerTable
calculate m' =
    foldl (\m1 row ->
        foldl (\m2 column ->
            updateMap m2 row column
        ) m1 (columns row)
    ) m' rows


rows :: [Integer]
rows = [1..fromIntegral $ length dataset]

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
