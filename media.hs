avg :: Int -> Int -> Int -> Double
avg = \x y z -> fromIntegral(x+y+z) / 3


--addTwoElements 2 12 [85,0,6]
--[2,12,85,0,6]

--addTwoElements x y l = x:y:l

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements = \x y l -> x:y:l


const42 :: a -> Int
const42 = const 42
