class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString False = "false"
    toString True = "true"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++toString (a) ++ "," ++ toString (b) ++ ")"



--Пусть существуют два класса типов KnownToGork и KnownToMork,
--которые предоставляют методы stomp (stab) и doesEnrageGork (doesEnrageMork) соответственно:

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

--Класса типов KnownToGorkAndMork является расширением обоих этих классов,
--предоставляя дополнительно метод stompOrStab:

--Задайте реализацию по умолчанию метода stompOrStab,
--которая вызывает метод stomp, если переданное ему значение приводит в ярость Морка;
--вызывает stab, если оно приводит в ярость Горка и вызывает сначала stab, а потом stomp,
--если оно приводит в ярость их обоих. Если не происходит ничего из вышеперечисленного,
--метод должен возвращать переданный ему аргумент.
--Класса типов KnownToGorkAndMork является расширением обоих этих классов,
--предоставляя дополнительно метод stompOrStab:

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | (doesEnrageGork a, doesEnrageMork a) == (True, False)  = stab a
                  | (doesEnrageGork a, doesEnrageMork a) == (False, True)  = stomp a
                  | (doesEnrageGork a, doesEnrageMork a) == (True, True)   = stomp (stab a)
                  | (doesEnrageGork a, doesEnrageMork a) == (False, False) = a