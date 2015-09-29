module Integrate where


--Реализуйте функцию, находящую значение определённого интеграла от заданной функции f на
--заданном интервале [a,b] методом трапеций. (Используйте равномерную сетку; достаточно 1000 элементарных отрезков.)
--
--integration :: (Double -> Double) -> Double -> Double -> Double
--integration f a b = undefined
--GHCi> integration sin pi 0
-- -2.0
--Результат может отличаться от -2.0, но не более чем на 1e-4.



integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b | a > b = (-1)*trap b a
                  | a < b = trap a b
                  | a == b = 0  where
    trap a b = (b-a)/n * ((f(a) + f(b))/2 + (sum $ map f list))
    dx = (b-a)/n
    n = 1000
    list = [a+dx, a + dx + dx .. b - dx]