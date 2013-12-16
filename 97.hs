import Data.Bits

main = print . reverse . take 10 . reverse . show $ (28433 * (shiftL 1 7830457)) + 1