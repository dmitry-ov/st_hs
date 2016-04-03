--ff xs = do
-- x <- xs
-- y <- map (+1) xs
-- return [x, y]


{- Используя монаду списка и do-нотацию, реализуйте функцию

 pythagoreanTriple :: Int -> [(Int, Int, Int)]

  которая принимает на вход некоторое число x и возвращает список троек (a,b,c)(a,b,c), таких что

 a2+b2=c2, a>0, b>0, c>0, c≤x, a<b

 Число x может быть ≤0 , на таком входе должен возвращаться пустой список.

 GHCi> pythagoreanTriple 5
 [(3,4,5)]

 GHCi> pythagoreanTriple 0
 []

 GHCi> pythagoreanTriple 10
 [(3,4,5),(6,8,10)]
 -}

  pythagoreanTriple :: Int -> [(Int, Int, Int)]
  pythagoreanTriple x = do
   a <- [1..x]
   b <- [1..x]
   c <- [1..x]
   True <- return (a < b)
   True <- return (c <= x)
   True <- return (a*a + b*b == c*c)
   return (a,b,c)
