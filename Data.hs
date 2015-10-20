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






















































