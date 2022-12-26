module Primos where

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