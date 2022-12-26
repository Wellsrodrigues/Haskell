module Teste where


-- Questao 01

aux :: Char -> [Char] -> Int
aux _ [] = 0
aux simb (a:ls)
    | simb == a = 1 + aux simb ls
    | otherwise = aux simb ls

analise :: [Char] -> [(Char, Int)]
analise lista = [(x,y) | x <- lista, y <- [aux x lista] ]


-- Questão 02

naoRepete :: String -> String
naoRepete [] = []
naoRepete [a] = [a]
naoRepete (a:ls)
    | a /= head ls = a : naoRepete ls
    | a == head ls = naoRepete ls

descresente :: [(Char, Int)] -> [(Char, Int)]
descresente [] = []
descresente ((a,b):l) = descresente [(x,y) | (x,y) <- l, y > b] ++ [(a,b)] ++ descresente[(x,y) | (x,y) <-l, y <= b]

vetor ::  [(Char, Int)] -> [Char]
vetor lista = [ x | (x,y) <- lista]

ordenaFreq :: [Char] -> [Char]
ordenaFreq lista = naoRepete (vetor (descresente (analise lista)))

