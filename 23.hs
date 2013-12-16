module Main where

import Math.Combinat.Sets
import Data.Time.Clock
import Data.List
import qualified Data.IntSet as IS

main :: IO ()
main = print $ sum $ (filter (flip IS.notMember pairs) boundary) ++ [1..11]

divisorSum :: Int -> Int
divisorSum n = sum . nub . filter ((==0) . mod n) $ ([div n x | x <- [2..8]] ++ [1..div n 9])

boundary = [12..28123]

abundand :: [Int]
abundand = map fst $ filter (\(a, b) -> a < b ) $ zip boundary (map divisorSum boundary)

pairs :: IS.IntSet
pairs = IS.fromList $ map sum $ combine 2 abundand