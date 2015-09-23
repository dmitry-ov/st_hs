module Demo where

import Data.Char

test = isDigit '7'


twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y
                    then (digitToInt x)*10 + (digitToInt y)
                    else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist x y = sqrt((fst x - fst y)^2 + (snd x - snd y)^2)


