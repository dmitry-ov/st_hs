data Log a = Log [String] a  deriving Show
--Реализуйте вычисление с логированием, используя Log. Для начала определите функцию toLogger
--
--toLogger :: (a -> b) -> String -> (a -> Log b)

--которая превращает обычную функцию, в функцию с логированием:
--
--let add1Log = toLogger (+1) "added one"
-- add1Log 3
--Log ["added one"] 4
--
--let mult2Log = toLogger (* 2) "multiplied by 2"
--GHCi> mult2Log 3
--Log ["multiplied by 2"] 6
--Далее, определите функцию execLoggers

--execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
--execLoggers
--Которая принимает некоторый элемент и две функции с логированием. execLoggers возвращает результат последовательного применения функций к элементу и список сообщений, которые были выданы при применении каждой из функций:
--GHCi> execLoggers 3 add1Log mult2Log
--Log ["added one","multiplicated by 2"] 8


toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f s x = Log [s] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g =  Log list gx where
                   Log fs fx = f x
                   Log gs gx = g fx
                   list = fs ++ gs

--Функции с логированием из предыдущего задания возвращают в качестве результата
--значение с некоторой дополнительной информацией в виде списка сообщений.
--Этот список является контекстом. Реализуйте функцию returnLog

--returnLog :: a -> Log a
--которая является аналогом функции return для конекста Log.
--Данная функция должна возвращать переданное ей значение в контексте с пустым списком.
add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"



returnLog :: a -> Log a
returnLog xs = Log [] xs

--Реализуйте фукцию bindLog
--которая работает подобно оператору >>= для контекста Log.
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog =(\(Log string x) f ->  let
                                Log list fx = f x
                               in
                                Log (string ++ list) fx)


--GHCi> Log ["nothing done yet"] 0 `bindLog` add1Log
--Log ["nothing done yet","added one"] 1
--
--GHCi> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
--Log ["nothing done yet","added one","multiplied by 2"] 8
--

--Реализованные ранее returnLog и bindLog позволяют определить тип Log представителем класса Monad:

instance Monad Log where
    return = returnLog
    (>>=) = bindLog
--Используя return и >>=, определите функцию execLoggersList
--

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a = foldl (>>=) (return a)

--которая принимает некоторый элемент, список функций с логированием и возвращает результат последовательного применения всех функций в списке к переданному элементу вместе со списком сообщений, которые возвращались данными функциями:
--
--GHCi> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
--Log ["added one","multiplied by 2","multiplied by 100"] 800
--
--
--execLoggersList :: a -> [a -> Log a] -> Log a
--execLoggersList = undefined















