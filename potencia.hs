import InteiroPositivo

potencia :: Int -> Int -> Int
potencia n e
    | e == 0 = 1
    | e == 1 = n
    | inteiroPositivo e = n * potencia n (e-1)
    | otherwise = -1
                