module Vendas where

--------------------------------------------------------------

--Vendas por semana

venda :: Int -> Int
venda 0 = 0
venda 1 = 6
venda 2 = 14
venda 3 = 20

--------------------------------------------------------------

-- Total de vendas 

totalVendas :: Int -> Int
totalVendas n
    |n == 0 = venda 0
    |otherwise = venda n + totalVendas (n-1)

--------------------------------------------------------------

-- Maior Venda entre as semanas

aux :: Int -> Int -> Int
aux m j
    |j == m = venda m
    |venda m  >= venda j = aux m (j+1)
    |otherwise = aux j 0

maiorVenda :: Int -> Int
maiorVenda x
    |x == 0 = venda 0
    |otherwise = aux x 0

--------------------------------------------------------------

-- Semana com maior Venda

auxx :: Int -> Int -> Int
auxx m j
    |j == m = j
    |venda m  >= venda j = auxx m (j+1)
    |otherwise = auxx j 0

semana :: Int -> Int
semana 0 = 0
semana x = auxx x 0

--------------------------------------------------------------

-- Semana sem venda

ehZero :: Int -> Bool
ehZero x
    |venda x == 0 = True
    |x == 0 = False
    |otherwise = ehZero (x-1)

--------------------------------------------------------------