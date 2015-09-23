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


fibonacci :: Integer -> Integer
fibonacci n | n > 0     = fibonacci(n-1) + fibonacci(n-2)
            | n == 1    = 1
            | n == 0    = 0
            | n == (-1) = 1
            | n == (-2) = (-1)
            | n < 0  = if even n
                       then (-1) * fibonacci(abs n)
                       else fibonacci(abs n)