module Reposicao where

----------------------------------------------------- IMPORTACOES -----------------------------------------------------------------------

auxPrimo :: Int -> Int -> Bool
auxPrimo num divisor
    |divisor == 1 = True
    |mod num divisor == 0 = False
    |otherwise = auxPrimo num (divisor-1)
    
primo :: Int -> Bool
primo 1 = False
primo x = auxPrimo x (x-1)


contPrimo :: Int -> Int -> Int
contPrimo cont numPrimo
    |cont == 0 = numPrimo-1
    |primo numPrimo = contPrimo (cont-1) (numPrimo+1)
    |otherwise = contPrimo cont (numPrimo+1)

nPrimo :: Int -> Int
nPrimo n = contPrimo n 2

----------------------------------------------------- QUESTAO 01 -----------------------------------------------------------------------

-- Obs: Os primeiros números afortunados são: 3, 5, 7, 13, 23, 17, 19, 23, 37, 61, 67, 61, 71, 47, ...

prodPrimo :: Int -> Int
prodPrimo p
    | p == 1 = 2
    | otherwise = nPrimo p * prodPrimo (p-1) 

aux :: Int -> Int -> Int
aux prod m
    | primo (prod + m) = m
    | otherwise = aux prod (m+1)

afortunado :: Int -> Int
afortunado n = aux (prodPrimo n) 2


----------------------------------------------------- QUESTAO 02 -----------------------------------------------------------------------

-- getByIndex [2,3,1,0] "IFMA" -> "MAFI"
-- getByIndex [1,-3,1,7,0,2] "Caxias" -> "a*a*Cx"

getByIndex :: [Int] -> String -> String
getByIndex indices lista  = [ if x `elem` indexx then lista !! x else '*' | x <- indices ]
    where
        indexx = [0 .. (length lista-1)]


----------------------------------------------------- QUESTAO 03 -----------------------------------------------------------------------

pascal :: Int -> [(Int, Int)]
pascal num = [(num, x) | x <- [0 .. num]]