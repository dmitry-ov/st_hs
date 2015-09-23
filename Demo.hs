module Demo where

import Data.Char

test = isDigit '7'


--twoDigits2Int '4' '2'

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if (isDigit x && isDigit y)
                        then (digitToInt x)*10 + (digitToInt y)
                            else 100

