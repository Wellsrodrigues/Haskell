module Avaliacao where
import Primos ( primo )


-- primos gêmeos são dois números primos cuja diferença é igual a dois.

primosGemeos :: Int -> Int -> Bool
primosGemeos x y
    | primo x && primo y && abs (x - y) == 2 = True
    | otherwise = False

-- pertence a algum par de número primo gêmeo.

ehPrimoGemeo :: Int -> Bool
ehPrimoGemeo p
    | p == 2 = False
    | primo p && primo (p-2) == True = True
    | primo p && primo (p+2) == True = True
    | otherwise = False

-- Contabilize quantos pares de primos gêmeos existem abaixo de um número n.

contPrimosGemeos :: Int -> Int
contPrimosGemeos n
    | n == 0 = 0
    | primo n == False = contPrimosGemeos (n-1)
    | ehPrimoGemeo n && primo (n-2) == True = 1 + contPrimosGemeos (n-2)
    | otherwise = contPrimosGemeos (n-2)


  --  Calcule a soma de todos os primos gêmeos que aparecem entre 0 e n.

somaPrimosGemeos :: Int -> Int
somaPrimosGemeos n
    | n == 0 = 0
    | primo n == False = somaPrimosGemeos (n-1)
    | ehPrimoGemeo n && primo (n-2) == True = n + (n-2) + somaPrimosGemeos(n-2)
    | otherwise = somaPrimosGemeos(n-2)