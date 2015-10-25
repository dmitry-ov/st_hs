import Demo


data Nat = Zero | Suc Nat  deriving Show

--Элементы этого типа имеют следующий вид:
--Zero, Suc Zero, Suc (Suc Zero), Suc (Suc (Suc Zero)), и так далее.
--Таким образом мы можем считать, что элементы этого типа - это натуральные числа в
--унарной системе счисления.
--
--Мы можем написать функцию, которая преобразует Nat в Integer следующим образом:

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add  = \a b -> toNat $ fromNat a + fromNat b

mul :: Nat -> Nat -> Nat
mul = \a b -> toNat $ fromNat a * fromNat b

fac :: Nat -> Nat
fac = \a -> toNat $ factorial $ fromNat a where
            factorial :: Integer -> Integer
            factorial n | n == 0    = 1
                        | n > 0     = n * factorial(n-1)
                        | otherwise = error "arg must be >=0"


toNat :: Integer -> Nat
toNat 0 = Zero
toNat x = Suc (toNat(x-1))
