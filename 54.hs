module Main where
import System.TimeIt
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.List
import Math.NumberTheory.Primes
import Debug.Trace
import Control.Monad
import Data.Function

file = readFile "C:/Users/kostas/Downloads/poker.txt"

data Winner = Player1 | Player2 | None
    deriving (Show, Eq)

type Kind = Char
type Value = Int
type Card = (Kind, Value)

main :: IO ()
main = do
    cardLists <- toCardLists file
    timeIt . print $ length $ filter (== Player1) $ map getWinner cardLists
--main = print t0

compareHands :: ([Card], [Card]) -> Winner
compareHands hands@(leftHand, rightHand) =
    if (length remaining == 0) then
        higherCard (highCard leftHand) (highCard rightHand)
    else
        head remaining
    where
        remaining = dropWhile (== None) (map (compare' hands) functions)

compare' :: ([Card], [Card]) -> (([Card] -> Bool), ([Card] -> [Card] -> Winner)) -> Winner
compare' (leftHand, rightHand) function
    | l == True && r == True        = (snd function) leftHand rightHand
    | l == True && r == False       = Player1
    | l == False && r == False      = None
    | otherwise                     = Player2
    where
        l = (fst function) leftHand
        r = (fst function) rightHand

functions = [
    (hasRoyalFlush, fallbackRoyalFlush),
    (hasStraightFlush, fallbackStraightFlush),
    (hasFourOfAKind, fallbackFourOfAKind),
    (hasFullHouse, fallbackFullHouse),
    (hasFlush, fallbackFlush),
    (hasStraight, fallbackStraight),
    (hasThreeOfAKind, fallbackThreeOfAKind),
    (hasTwoPairs, fallbackTwoPairs),
    (hasOnePair, fallbackOnePair),
    (\_ -> False, \_ _ -> Player1)
    ]

t0 = higherCard ('a', 5) ('b', 9)
t1 = highCard [('a', 4), ('a', 7), ('a', 5)]
t2 = hasOnePair [('a', 4), ('a', 7), ('a', 5), ('a', 5), ('a', 3)]
t3 = hasTwoPairs [('a', 3), ('a', 7), ('a', 5), ('a', 5), ('a', 3)]
t4 = hasThreeOfAKind [('a', 5), ('b', 7), ('b', 5), ('a', 5), ('a', 3)]
t5 = hasStraight [('a', 5), ('b', 7), ('b', 6), ('a', 3), ('a', 4)]
t6 = hasFlush [('a', 5), ('a', 7), ('a', 6), ('a', 3), ('a', 4)]
t7 = hasFullHouse [('a', 5), ('a', 5), ('a', 5), ('a', 3), ('a', 3)]
t8 = hasFourOfAKind [('a', 5), ('a', 5), ('a', 5), ('a', 5), ('a', 4)]
t9 = hasStraightFlush [('a', 5), ('a', 7), ('a', 6), ('a', 3), ('a', 4)]
t10 = hasRoyalFlush [('a', 11), ('a', 12), ('a', 13), ('a', 14), ('a', 10)]

getWinner :: [Card] -> Winner
getWinner cards = compareHands $ splitAt 5 cards

toCardLists :: IO String -> IO [[Card]]
toCardLists file = fmap (fmap (map toCard . words) . lines) file

toValue :: Char -> Value
toValue c = snd . head . filter ((== c) . fst) $ vals

vals = zip "23456789TJQKA" [2..]

toCard :: [Char] -> Card
toCard [a, b] = (b, toValue a)

higherCard :: Card -> Card -> Winner
higherCard (k1, v1) (k2, v2) = if (v1 > v2) then Player1 else Player2

highCard :: [Card] -> Card
highCard cards = head . reverse . sortBy (comparing snd) $ cards

hasOnePair :: [Card] -> Bool
hasOnePair cards = ofValue (map snd cards) 1 2

fallbackOnePair left right
    | lt > rt   = Player1
    | lt < rt   = Player2
    | lt == rt  = higherCard (highCard left) (highCard right)
    where
        lt = head . filter ((==2) . length) $ group $ sort (map snd left)
        rt = head . filter ((==2) . length) $ group $ sort (map snd right)

hasTwoPairs :: [Card] -> Bool
hasTwoPairs cards = ofValue (map snd cards) 2 2

-- lets leave this as a warning
fallbackTwoPairs :: [Card] -> [Card] -> Winner
fallbackTwoPairs left right = if (lt > rt) then Player1 else Player2
    where
        lt = head . reverse . filter ((==2) . length) $ group $ sort (map snd left)
        rt = head . reverse . filter ((==2) . length) $ group $ sort (map snd right)

hasThreeOfAKind :: [Card] -> Bool
hasThreeOfAKind cards = ofValue (map snd cards) 1 3

fallbackThreeOfAKind :: [Card] -> [Card] -> Winner
fallbackThreeOfAKind = fallbackFullHouse

hasStraight :: [Card] -> Bool
hasStraight = areConsecutive . map snd

fallbackStraight :: [Card] -> [Card] -> Winner
fallbackStraight = fallbackStraightFlush

hasFlush :: [Card] -> Bool
hasFlush = allSameSuit

fallbackFlush :: [Card] -> [Card] -> Winner
fallbackFlush left right = higherCard (highCard left) (highCard right)

hasFullHouse :: [Card] -> Bool
hasFullHouse cards = ofValue vals 1 3 && ofValue vals 1 2
    where
         vals = map snd cards

fallbackFullHouse :: [Card] -> [Card] -> Winner
fallbackFullHouse left right = if (lt > rt) then Player1 else Player2
    where
        lt = head . filter ((==3) . length) $ group $ sort (map snd left)
        rt = head . filter ((==3) . length) $ group $ sort (map snd right)

hasFourOfAKind :: [Card] -> Bool
hasFourOfAKind cards = ofValue (map snd cards) 1 4

fallbackFourOfAKind :: [Card] -> [Card] -> Winner
fallbackFourOfAKind left right = if (lc > rc) then Player1 else Player2
    where
        lc = head . filter ((==4) . length) $ group $ sort (map snd left)
        rc = head . filter ((==4) . length) $ group $ sort (map snd right)

hasStraightFlush :: [Card] -> Bool
hasStraightFlush cards = allSameSuit cards && areConsecutive (map snd cards)

fallbackStraightFlush :: [Card] -> [Card] -> Winner
fallbackStraightFlush left right = if (lc > rc) then Player1 else Player2
    where
        lc = head . sort . map snd $ left
        rc = head . sort . map snd $ right

hasRoyalFlush :: [Card] -> Bool
hasRoyalFlush cards = allSameSuit cards && containsValues "TJQKA" cards

fallbackRoyalFlush :: [Card] -> [Card] -> Winner
fallbackRoyalFlush left right = Player1

allSameSuit :: [Card] -> Bool
allSameSuit cards = all ((== suit) . fst) cards
    where
        suit = fst . head $ cards

containsKinds :: [Char] -> [Card] -> Bool
containsKinds kinds cards = contains kinds (map fst cards)

containsValues :: [Char] -> [Card] -> Bool
containsValues chars cards = contains (map toValue chars) (map snd cards)

contains :: (Eq a) => [a] -> [a] -> Bool
contains a b = (== 0) . length $ a \\ b

areConsecutive :: [Int] -> Bool
areConsecutive values = sorted == [first..first + 4]
    where
        sorted = sort values
        first = head sorted

ofValue :: (Eq a, Ord a) => [a] -> Int -> Int -> Bool
ofValue vals groupCount groupSize = (== groupCount) . length . filter ((== groupSize) . length) $ group $ sort vals
