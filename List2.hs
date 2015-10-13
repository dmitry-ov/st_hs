module List2 where

--Пусть задан тип Odd нечетных чисел следующим образом:

data Odd = Odd Integer
  deriving (Eq, Show)

instance Enum (Odd) where
    succ x = x + 2
    pred x = x - 2

    toEnum (Odd n)
                | n mod 2 == 0  = n
                | otherwise     = error "error. not odd"
    fromEnum (Odd n)
                | n mod 2 == 0  = n
                | otherwise     = error "error. not odd"



addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"



--Сделайте этот типа представителем класса типов Enum.

--GHCi> succ $ Odd (-100000000000003)
--Odd (-100000000000001)

