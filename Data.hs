module Data where

import Prelude

--data Bool = True | False
--
--alwaysTrue :: Int -> Bool
--alwaysTrue n = True
--
--
--data B = T | F deriving (Show, Eq, Read, Enum)
--
--not' :: B -> B
--not' T = F
--not' F = T




--GHCi> show Red
--"Red"
--
--1
--instance Show Color where
--2
--    show = undefined


data Color = Red | Green | Blue

instance Show Color where
    show Red  =  "Red"
    show Green =  "Green"
    show Blue  =  "Blue"
--    Show Green = "Green"
--    Show Blue = "Blue"

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue


--GHCi> charToInt '0'
--0
--GHCi> charToInt '9'
--9

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9



--Определите функцию cmp, сравнивающую элементы типа LogLevel так, чтобы было верно, что Error > Warning > Info.
--GHCi> cmp Error Warning
--GT
--GHCI> cmp Info Warning
--LT

data LogLevel = Error | Warning | Info

cmp Error Warning = GT
cmp Error Info = GT


cmp Warning Error = LT
cmp Warning Info  = GT

cmp Info Warning = LT
cmp Info Error = LT

cmp Info Info = EQ
cmp Error Error = EQ
cmp Warning Warning = EQ


--
--
--Пусть объявлен следующий тип данных:
--
--data Result = Fail | Success
--

--И допустим определен некоторый тип данных SomeData и некоторая функция

--doSomeWork :: SomeData -> (Result,Int)

--возвращающая результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха.
--Определите функцию processData, которая вызывает doSomeWork и
--возвращает строку "Success" в случае ее успешного завершения, либо строку "Fail: N" в случае неудачи, где N — код ошибки.

--processData :: SomeData -> String
--processData someData = case doSomeWork someData of
--                 (Success, 0) -> "Success"
--                 (Fail, n) -> "Fail: " ++ show n


--data Shape = Circle Double | Rectangle Double Double
--
--area :: Shape -> Double
--area shape = case shape of
--              (Circle r) -> pi*r^2
--              (Rectangle x y) -> x*y
--






--В одном из прошлых заданий мы встречали тип Result и функцию doSomeWork:
--
--data Result = Fail | Success
--
--doSomeWork :: SomeData -> (Result,Int)

--Функция doSomeWork возвращала результат своей работы и либо код ошибки в
--случае неудачи, либо 0 в случае успеха. Такое определение функции
--не является наилучшим, так как в случае успеха мы вынуждены
--возвращать некоторое значение, которое не несет никакой смысловой нагрузки.
--
--Используя функцию doSomeWork, определите функцию doSomeWork' так,
--чтобы она возвращала код ошибки только в случае
--неудачи. Для этого необходимо определить тип Result'. Кроме того, определите instance Show для Result' так,
--чтобы show возвращал "Success" в случае успеха и "Fail: N" в случае неудачи, где N — код ошибки.
--

--data Result = Fail | Success
--data Result' = Result Int
--
--instance Show Result' where
--    show (Result n) = case n of
--                       0 -> "Success"
--                       otherwise -> "Fail: " ++ show n
--
--doSomeWork' :: SomeData -> Result'
--doSomeWork' someData = case doSomeWork someData of
--                          (Success, 0)  -> (Result 0)
--                          (Fail, n )  -> (Result n)
--



data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _ = False