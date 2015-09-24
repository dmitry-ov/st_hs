module Demo where

import Data.Char

test = isDigit '7'


twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y
                    then (digitToInt x)*10 + (digitToInt y)
                    else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist x y = sqrt((fst x - fst y)^2 + (snd x - snd y)^2)


--factorial :: Int -> Int
--factorial 0 = 0
--factorial n = factorial (n-1)

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact(n-2)



factorial :: Integer -> Integer
factorial n | n == 0    = 1
            | n > 0     = n * factorial(n-1)
            | otherwise = error "arg must be >=0"


--factorial5 n | n > 0 = helper 1 n
--             | otherwise = error "arg must be >= 0"

--helper acc 0 = acc
--helper acc n = helper (acc * n) (n-1)


fibonacci :: Integer -> Integer
fibonacci n | n > 0     =  help 0 1 n
            | n == 1    = 1
            | n == 0    = 0
            | n == (-1) = 1
            | n == (-2) = (-1)
            | n < 0  = if even n
                       then (-1) * help 0 1 (abs n)
                       else help 0 1 (abs n)

--fibonacci 10 = 55
--fibonacci (-10) = (-55)

help :: Integer -> Integer -> Integer -> Integer
help acc1 acc2 n = if (n == 0) then acc1 else help acc2 (acc1 + acc2) (n-1)

























