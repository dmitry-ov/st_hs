--Введём следующий тип:
data Log a = Log [String] a  deriving Show

--Реализуйте вычисление с логированием, используя Log. Для начала определите функцию toLogger
--
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f s x = Log [s] (f x)

--которая превращает обычную функцию, в функцию с логированием:
--
--GHCi> let add1Log = toLogger (+1) "added one"
--GHCi> add1Log 3
--Log ["added one"] 4
--
--GHCi> let mult2Log = toLogger (* 2) "multiplied by 2"
--GHCi> mult2Log 3
--Log ["multiplied by 2"] 6
--Далее, определите функцию execLoggers
--


execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g =  Log list gx where
                   Log fs fx = f x
                   Log gs gx = g fx
                   list = fs ++ gs


--Которая принимает некоторый элемент и две функции с логированием.
--execLoggers возвращает результат последовательного применения функций
--к элементу и список сообщений, которые были выданы при применении каждой из функций:
--GHCi> execLoggers 3 add1Log mult2Log
--Log ["added one","multiplicated by 2"] 8

returnLog :: a -> Log a
returnLog = Log []


