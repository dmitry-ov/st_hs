module Demo where

import Data.Char

test = isDigit '7'

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y
                    then (digitToInt x)*10 + (digitToInt y)
                    else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist x y = sqrt((fst x - fst y)^2 + (snd x - snd y)^2)

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact(n-2)

factorial :: Integer -> Integer
factorial n | n == 0    = 1
            | n > 0     = n * factorial(n-1)
            | otherwise = error "arg must be >=0"


factorial5 n | n > 0 = helper 1 n
             | otherwise = error "arg must be >= 0"

helper acc 0 = acc
helper acc n = helper (acc * n) (n-1)


fibonacci :: Integer -> Integer
fibonacci n | n > 0     =  help 0 1 n
            | n == 1    = 1
            | n == 0    = 0
            | n == (-1) = 1
            | n == (-2) = (-1)
            | n < 0  = if even n
                       then (-1) * help 0 1 (abs n)
                       else help 0 1 (abs n)

help :: Integer -> Integer -> Integer -> Integer
help acc1 acc2 0 = acc1
help acc1 acc2 n = help acc2 (acc1 + acc2) (n-1)

-- take 30 $ fix ((1:) . scanl (+) 1)
--[1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040]


--Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности
--a0=1; a1=2; a2=3; ak+3=ak+2+ak+1−2ak.

--seqA 301
--1276538859311178639666612897162414
--Попытайтесь найти эффективное решение.

seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | n > 2  =  let
                     recurrently acc1 acc2 acc3 0 = acc1
                     recurrently acc1 acc2 acc3 n = recurrently acc2 acc3 ((acc3 + acc2) - 2*acc1) (n-1)
                   in recurrently 1 2 3 n
