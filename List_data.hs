data List a = Nil | Cons a (List a)  deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x Nil) = [x]
fromList (Cons x xs) =  x : fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)