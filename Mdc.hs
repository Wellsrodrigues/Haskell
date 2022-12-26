module Mdc where
import InteiroPositivo ( inteiroPositivo )

mdc :: Int -> Int -> Int
mdc x y
    | inteiroPositivo x && inteiroPositivo y && x == y = y
    | inteiroPositivo x && inteiroPositivo y && x > y = mdc y (x-y)
    | inteiroPositivo x && inteiroPositivo y && x < y = mdc x (y-x)
    | otherwise = -1