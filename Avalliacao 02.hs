module Avaliacao 02 where



aux :: Char -> [Char] -> Int
aux _ [] = 0
aux simb (a:ls)
    | simb == a = 1 + aux simb ls

analise :: [Char] -> [(Char, Int)]
analise [] = []