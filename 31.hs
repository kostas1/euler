module Main where
import System.TimeIt

main :: IO ()
main = timeIt . print $ length $ combinations 200


combinations l = [
    [] |
    a <- size 200 l,
    b <- size 100 (l - a),
    c <- size 50  (l - a - b),
    d <- size 20  (l - a - b - c),
    e <- size 10  (l - a - b - c - d),
    f <- size 5   (l - a - b - c - d - e),
    g <- size 2   (l - a - b - c - d - e - f),
    h <- size 1   (l - a - b - c - d - e - f - g),
    a + b + c + d + e + f + g + h ==  l
    ]
    where
        size n l = [0, n..l]