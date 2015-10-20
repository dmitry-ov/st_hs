module List2 where

data Odd = Odd Integer
  deriving (Eq, Show)

instance Enum Odd where

    succ (Odd n) = Odd (n + 2)
    pred (Odd n) = Odd (n - 2)

    toEnum n = if odd n then (Odd $ fromIntegral n) else error "error, not odd"
    fromEnum (Odd n) = fromIntegral n





addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"



--Сделайте этот типа представителем класса типов Enum.

--GHCi> succ $ Odd (-100000000000003)
--Odd (-100000000000001)


