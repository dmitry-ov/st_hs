sign x = if x > 0 then 1
            else
                if x < 0 then -1
                    else 0



--sign' x =
--    case (x) of
--       _ | x > 1 -> 1
--         | x < 1 -> -1
--         |otherwise - > 0


sign' x
    | x > 0  = 1
    | x < 0 = -1
    | otherwise = 0
