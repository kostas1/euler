module Main where
import System.TimeIt

main :: IO ()
main = timeIt $ print . length . filter isNth $ vars

isNth (a, b, c) = (== b) . digitCount $ c

digitCount :: Integer -> Integer
digitCount = fromIntegral . length . show

vars = [(x, y, x^y) | x <- [1..9], y <- [1..30]]