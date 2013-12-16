calc :: Integer -> Integer -> Integer -> Integer
calc 0 b p =    p
calc a b p =    let p' = if (a .&. 1 == 1) then p + b else p
                in calc (shiftR a 1) (shiftL b 1) p'