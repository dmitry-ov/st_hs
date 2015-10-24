data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) =  abs(x1-x2) + abs(y1-y2)


getCenter :: Double -> Coord Int -> Coord Double
getCenter width (Coord x y) = (Coord centerX centerY) where
                                 d = width/2
                                 centerX = width * fromIntegral x + d
                                 centerY = width * fromIntegral y + d

getCell :: Double -> Coord Double -> Coord Int
getCell width (Coord x y) = (Coord numX numY) where
                                numX = floor (x / width)
                                numY = floor (y / width)


--getCenter 1 (Coord 0 0) == Coord 0.5 0.5

--getCell 10 (Coord 23 47) == Coord 2 4
--getCell 0,5 (Coord 2,25 3,25) == Coord 4 6