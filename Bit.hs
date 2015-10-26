--Целое число можно представить как список битов со знаком.
--
--Реализуйте функции сложения и умножения для таких целых чисел,
--считая, что младшие биты идут в начале списка, а старшие — в конце.

data Bit = Zero | One  deriving Show
data Sign = Minus | Plus deriving Show
data Z = Z Sign [Bit] deriving Show

add :: Z -> Z -> Z
add = (\a b -> intToZ  $ zToInt a + zToInt b)

mul :: Z -> Z -> Z
mul =(\a b -> intToZ  $ zToInt a * zToInt b)


zToInt (Z Minus xs) = (-1) * (num xs)
zToInt (Z Plus xs) = num  xs

num :: [Bit] -> Int
num xs = binList2Int $ bit2ListInt xs where
        bit2ListInt list = b2iList [] list

        b2iList acc [] = acc
        b2iList acc (x:xs) = b2iList (acc ++ [(change x)]) xs

        change Zero = 0
        change One = 1

        binList2Int :: [Int] -> Int
        binList2Int list =  bi2i 0 0 list

        bi2i :: Int -> Int -> [Int] -> Int
        bi2i acc _ [] = acc
        bi2i acc n (x:xs) = bi2i (acc + x * (2^n)) (n+1) xs


intToZ n | n == 0 =  Z Plus [Zero]
         | n < 0  =  Z Minus (i2z(abs n))
         | n > 0 = Z Plus (i2z(abs n))  where
            i2z i = map change' $ toBinList i
            change' 0 = Zero
            change' 1 = One


toBinList :: Int -> [Int]
toBinList 0 = [0]
toBinList n = reverse $ tail $ toBin n  where
                toBin 0 = [0]
                toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
                        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]