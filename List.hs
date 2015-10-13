module List where

import Data.Char

--import Data.List

--GHCi> nTimes 42 3
--[42,42,42]
--GHCi> nTimes 'z' 5
--"zzzzz"


nTimes :: a -> Int -> [a]
nTimes = \x c -> times [] x c


times :: [a] -> a -> Int -> [a]
times list x 0 = list
times list x c = times (x:list) (x) (c-1)


oddsOnly :: Integral a => [a] -> [a]
oddsOnly = \list -> [x | x <- list, odd x]


isPalindrome :: Eq a => [a] -> Bool
isPalindrome = \list -> list == reverse list


--GHCi> sum3 [1,2,3] [4,5] [6]
--[11,7,3]

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 a b c =  sum2 a $ sum2 b c

sum2 :: Num a => [a] -> [a] -> [a]
sum2 [] [] = []
sum2 (x:xs) [] = x : sum2 xs []
sum2 [] (y:ys) = y : sum2 ys []
sum2 (x:xs) (y:ys) = (x+y) : sum2 xs ys

--
--GHCi> groupElems []
--[]
--GHCi> groupElems [1,2]
--[[1],[2]]
--GHCi> groupElems [1,2,2,4]
--[[1],[2,2],[4]]
--GHCi> groupElems [1,2,3,2,4]
--[[1],[2],[3],[2],[4]]

--groupElems :: Eq a => [a] -> [[a]]
--groupElems [] = [[]]
--groupElems list = help [[head list]] list
--
--
--help accG [] = accG
--help accG list = if (head list == (head $ tail list))
--                 then help (accG ++ [head list]) (tail list)
--                 else help (accG ++ fst(tuple)) snd(tuple) where
--
--                tuple = count [] (head list) (tail list)
--
--                count acc x list = if x `elem` list
--                                   then count (acc ++ [x]) x (tail list)
--                                   else ([acc], list)




groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = let (g,r) = collect [x] x xs
  in g : groupElems r
  where
    collect a l [] = (a, [])
    collect a l (x:xs) | l == x = collect (x : a) x xs
                       | otherwise = (a, x:xs)



--GHCi> readDigits "365ads"
--("365","ads")
--GHCi> readDigits "365"
--("365","")

readDigits :: String -> (String, String)
readDigits = \str -> span isDigit str

--Реализуйте функцию filterDisj, принимающую два унарных предиката и список,
--и возвращающую список элементов, удовлетворяющих хотя бы одному из предикатов.
--GHCi> filterDisj (< 10) odd [7,8,10,11,12]
--[7,8,11]


filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj f g = filter (\x -> f x || g x)

--Напишите функцию squares'n'cubes, принимающую список чисел,
--и возвращающую список квадратов и кубов элементов исходного списка.
--GHCi> squares'n'cubes [3,4,5]
--[9,27,16,64,25,125]



squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes= \list -> collect [] list where
                    f = (^2)
                    g = (^3)
                    collect acc [] = acc
                    collect acc (x:xs) = collect (acc ++ [f x] ++ [g x]) xs



--delAllUpper "Abc IS not ABC"
delAllUpper :: String -> String
delAllUpper = \string -> unwords . filter (or . map isLower)  $ words $ string


max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 a b c =  zipWith (max) c $ zipWith (max) a b


--perms [1,2,3]
perms :: [a] -> [[a]]
perms x = permutations x

-- > permutations "abc" == ["abc","bac","cba","bca","cab","acb"]

permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)

































