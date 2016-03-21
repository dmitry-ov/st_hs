data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

abbrFirstName :: Person -> Person
abbrFirstName p = Person {firstName = name, lastName = (lastName p), age = (age p)} where
                    name = if length (firstName p) < 2
                           then firstName p
                           else (first_char):'.':[]
                    first_char = head (firstName p)


updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = Person{ age = age p2, firstName = firstName p2, lastName = lastName p1 }
