module Haminng where
import Data.List


cont :: Eq a => a -> [a] -> Int
cont simb lst = sum [1 | x <- lst, x == simb]

freq :: Eq a => [a] -> [(a,Int)]
freq lst = nub [(x, cont x lst)| x <- lst]

---------------------------------------------------------------------------------

distance :: Eq a => [a] -> [a] -> Int
distance lst1 lst2 = sum [1 | i <- [0.. (length lst1 - 1)], (lst1 !! i) /= (lst2 !! i)] + abs (length lst1 - length lst2)

compara ::  String -> [String] -> [(String, Int)]
compara plv lst = [(n, valor) | n <- lst, valor <- [distance plv n]]

minimo :: (a, Int) -> [(a, Int)] -> (a, Int)
minimo (g,z) [] = (g,z)
minimo (g,z) ((a,b):x)
    | b < z = minimo (a,b) x
    | otherwise = minimo (g,z) x

hamming :: [String] -> [String] -> [(String, (String, Int))]
hamming str1 str2 = [(ls, menor) | ls <- str1, menor <- [minimo ("_", 99) (compara ls str2)]]

