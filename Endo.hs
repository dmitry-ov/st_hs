type Endo a = a -> a

func :: Endo (Endo Int) -> Int
func x = x (1+) 1
test = func (\x y -> x y)




newtype A a b = A a
newtype A = A A
newtype A a = A a
newtype A a b = A b