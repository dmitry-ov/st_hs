module List where


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

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = [[]]
groupElems list = help [[]] list


help accG [] = accG
help accG list = if (head list == (head $ tail list))
                 then help (accG ++ [head list]) (tail list)
                 else help (accG ++ fst(tuple)) snd(tuple) where
                
                tuple = count [] (head list) (tail list)) 

                count acc x list = if x `elem` list
                                   then count (acc ++ [x]) x (tail list)
                                   else ([acc], list)

