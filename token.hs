import Data.Char


data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)

--Реализуйте лексер арифметических выражений. Для начала реализуйте следующую функцию:

{-
Она проверяет, является ли переданная строка числом (используйте функцию isDigit из модуля Data.Char),
знаком "+" или "-", открывающейся или закрывающейся скобкой.
Если является, то она возвращает нужное значение обёрнутое в Just, в противном случае - Nothing:
-}
asToken :: String -> Maybe Token
asToken xs | (all isDigit xs) =  Just $ Number $ read xs
           | xs == "+"            = Just (Plus)
           | xs == "-"            = Just (Minus)
           | xs == "("            = Just (LeftBrace)
           | xs == ")"            = Just (RightBrace)
           | otherwise      = Nothing

{-Она проверяет, является ли переданная строка числом (используйте функцию isDigit из модуля Data.Char),
знаком "+" или "-", открывающейся или закрывающейся скобкой.
Если является, то она возвращает нужное значение обёрнутое в Just, в противном случае - Nothing:
-}

--Далее, реализуйте функцию tokenize:

tokenize :: String -> Maybe [Token]
tokenize l = sequence . map asToken $ words l

{-Функция принимает на вход строку и если каждое слово является корректным токеном,
то она возвращает список этих токенов, завёрнутый в Just. В противном случае возвращается Nothing.

Функция должна разбивать входную строку на отдельные слова по пробелам (используйте библиотечную функцию words).
Далее, полученный список строк должен быть свёрнут с использованием функции asToken и свойств монады Maybe:

GHCi> tokenize "1 + 2"
Just [Number 1,Plus,Number 2]

GHCi> tokenize "1 + ( 7 - 2 )"
Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace]

GHCi> tokenize "1 + abc"
Nothing
-}



