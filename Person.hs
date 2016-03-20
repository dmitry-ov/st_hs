--data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show
--
--abbrFirstName :: Person -> Person
--abbrFirstName p = Person {firstName = name, lastName = (lastName p), age = (age p)} where
--                    name = if length (firstName p) < 2
--                           then firstName p
--                           else (first_char):'.':[]
--                    first_char = head (firstName p)
--
--
--updateLastName :: Person -> Person -> Person
--updateLastName p1 p2 = Person{ age = age p2, firstName = firstName p2, lastName = lastName p1 }
--


--Реализуйте функцию parsePerson, которая разбирает строки вида
--firstName = John\nlastName = Connor\nage = 30 и возвращает либо результат типа Person, либо ошибку типа Error.

-- + Строка, которая подается на вход, должна разбивать по символу '\n' на список строк,
--каждая из которых имеет вид X = Y. Если входная строка не
--имеет указанный вид, то функция должна возвращать ParsingError.

--Если указаны не все поля, то возвращается IncompleteDataError.
--Если в поле age указано не число, то возвращается IncorrectDataError str, где str — содержимое поля age.
--Если в строке присутствуют лишние поля, то они игнорируются.
--Time Limit: 5 seconds
--Memory Limit: 256 MB


data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show
data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show


--"firstName = John\nlastName = Connor\nage = 30"
parsePerson :: String -> Either Error Person
parsePerson l = if not (hasEq $ lines l)
                then Left ParsingError
                else
                    if (3 /= (length $ lines l))
                    then Left IncompleteDataError
                    else
                        if not (hasFields $ lines l)
                        then IncompleteDataError
                        else

--


--  filter(/= '=') $ lines "firstName = John\nlastName = Connor\nage = 30"

--map (filter(/= '=')) (lines "firstName = John\nlastName = Connor\nage = 30")  => ["firstName  John","lastName  Connor","age  30"]


--  Right Person{age = 1, firstName = "sdd", lastName = "adddfg"}


--not (findEq (lines l !! 0)  &&  findEq (lines l !! 1) && findEq (lines l !! 2))

hasFields :: [String] -> Bool
hasFields l =


findName l = filter (== "firstName") l

listToPairs l

hasEq :: [String] -> Bool
hasEq l = foldl1 (&&) (map findEq l)


findEq :: String -> Bool
findEq [] = False
findEq (x:xs) = if (x == '=')
                then True
                else findEq xs


--tell :: (Show a) => [a] -> String
--tell [] = "The list is empty"
--tell (x:[]) = "The list has one element: " ++ show x
--tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
--tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
--tell ("age =":_) = "!!!!!"
--
--



