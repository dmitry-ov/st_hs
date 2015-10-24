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
