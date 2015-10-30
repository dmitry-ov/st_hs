import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) = if isDigit x
                   then Just x
                   else findDigit xs


findDigitOrX :: [Char] -> Char
findDigitOrX string = case findDigit string of
                        Just a -> a
                        Nothing -> 'X'



-- Maybe можно рассматривать как простой контейнер, например, как список длины 0 или 1.
-- Реализовать функции maybeToList и listToMaybe, преобразующие Maybe a в [a] и
-- наоборот (вторая функция отбрасывает все элементы списка, начиная со второго).


maybeToList :: Maybe a -> [a]
maybeToList a = case a of
                Nothing -> []
                Just a -> [a]

listToMaybe :: [a] -> Maybe a
listToMaybe a = if length a >= 1
                then Just (head a)
                else Nothing


eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right b) = Nothing