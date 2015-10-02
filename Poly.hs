module Poly where

import Data.Function

getSecondFrom :: a -> b -> c -> b
getSecondFrom x y z = y

mono :: Char -> Char
mono x = x

semiMono :: Char -> a -> Char
semiMono x y = x

apply2 f x = f (f x)

sumSquares = (+) `on` (^2)

--let p1 = ((1,2),(3,4))
--let p2 = ((3,4),(5,6))

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = (op) (f x) (f y) (f z)

sum3squares = (\x y z -> x+y+z) `on3` (^2)

--
--Функция одной переменной doItYourself выбирает наибольшее из переданного ей аргумента и числа 42,
--затем возводит результат выбора в куб и, наконец,
--вычисляет логарифм по основанию 2 от полученного числа. Эта функция реализована в виде:
--
--doItYourself = f . g . h
--Напишите реализации функций f, g и h. Постарайтесь сделать это в бесточечном стиле.


doItYourself = f . g . h

f = logBase 2

g = (^3)

h = max 42













































