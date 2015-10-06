--Реализуйте класс типов Printable, предоставляющий один метод toString — функцию одной переменной,
--которая преобразует значение типа, являющегося представителем Printable, в строковое представление.
--
--Сделайте типы данных Bool и () представителями этого класса типов, обеспечив следующее поведение:
--
--GHCi> toString True
--"true"
--GHCi> toString False
--"false"
--GHCi> toString ()
--"unit type"

--Сделайте тип пары представителем класса типов Printable, реализованного вами в предыдущей задаче,
-- обеспечив следующее поведение:
--
--GHCi> toString (False,())
--"(false,unit type)"
--GHCi> toString (True,False)
--"(true,false)"
--


class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString False = "false"
    toString True = "true"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++toString (a) ++ "," ++ toString (b) ++ ")"

