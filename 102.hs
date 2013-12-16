import System.Time
import Math.Core.Utils
import Data.List.Split (splitOn)

-- http://projecteuler.net/problem=102
-- takes around 650ms
main = do
    t1 <- getClockTime
    trianglesUnparsed <- fmap lines (readFile "file location")
    -- Mind-blowing way of parsing a file!
    let triangles = map (\c -> map (\(a:b:[]) -> (a,b) ) (groupsOf 2 c)) $ map (\b -> map (\c -> read c::Integer) (splitOn "," b)) trianglesUnparsed
    print $ length $ filter (==True) $ map isInside triangles
    t2 <- getClockTime
    print $ diffClockTimes t1 t2

triangleArea :: [(Integer, Integer)] -> Double
triangleArea ((x1, y1):(x2, y2):(x3, y3):[]) = do
    let a = x1 - x3
    let b = y1 - y3
    let c = x2 - x3
    let d = y2 - y3
    0.5*(abs( fromInteger ((a*d) - (b*c) )) )

groupsOf n = takeWhile (not.null) . map (take n) . iterate (drop n)

triangleCombinations :: [(Integer, Integer)] -> [[(Integer, Integer)]]
triangleCombinations set = map (\(a:b:[]) -> (a:b:(0,0):[])) $ combinationsOf 2 set

isInside :: [(Integer, Integer)] -> Bool
isInside orig = (triangleArea orig) == (sum $ map triangleArea $ triangleCombinations orig)