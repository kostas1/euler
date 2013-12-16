module Main where

import Data.List

type Year = Int
type Day = Int
type Weekday = Int


months :: Year -> [Day]
months y = [
    31,
    if (isLeap y) then 29 else 28,
    31,
    30,
    31,
    30,
    31,
    31,
    30,
    31,
    30,
    31]

isSunday :: Weekday -> Day -> Bool
isSunday janFirst day = mod (day + janFirst) 7 == 0

yearSundays :: (Year, Weekday) -> Day
yearSundays (year, janFirst) = length $ filter (isSunday janFirst) $ firstDays year

nextFirstJan :: Year -> Weekday -> Weekday
nextFirstJan year janFirst = yearDays' - (sundays * 7)
    where
        sundays = div (yearDays') 7
        yearDays' = yearDays year + janFirst

firstDays :: Year -> [Day]
firstDays year = init $ 1 : tail (scanl (+) 1 (months year))

yearFirsts :: [(Year, Weekday)]
yearFirsts = 
    init $ foldl (\last current -> 
        (current, nextFirstJan (fst . head $ last) (snd . head $ last)) : last) [(1900, 0)] [1901..2000]

yearDays :: Year -> Day
yearDays year
    | isLeap year   = 366
    | otherwise     = 365

isLeap :: Year -> Bool
isLeap year = mod year 400 == 0 || (mod year 4 == 0 && mod year 100 /= 0)

main :: IO ()
main = print . sum $ map yearSundays yearFirsts 