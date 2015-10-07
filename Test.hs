class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString False = "false"
    toString True = "true"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++toString (a) ++ "," ++ toString (b) ++ ")"


a = 127.2
b = 24.1
c = 20.1
d = 2

ip = show a ++ show b ++ show c ++ show d

--"127.224.120.12"