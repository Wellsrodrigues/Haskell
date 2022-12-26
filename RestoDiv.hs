module Restodiv where
import InteiroPositivo ( inteiroPositivo )

restoDiv :: Int -> Int -> Int
restoDiv x y
    | inteiroPositivo x && inteiroPositivo y &&  x == y || y == 1 = 0
    | inteiroPositivo x && inteiroPositivo y &&  x < y = x
    | inteiroPositivo x && inteiroPositivo y &&  x > y = x - y
    | otherwise = -1

