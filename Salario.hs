module Salario where


-- Funcao Id = salario

salario :: Int -> Float
salario 1 = 773.25
salario 2 = 2375.0
salario 3 = 1778.5
salario 4 = 6520.0
salario 5 = 3447.35
salario 6 = 5225.75
salario 7 = 8932.0
salario 8 = 648.5
salario 9 = 1982.4
salario 10 = 2157.45
salario _ = 0


-- Funcao gasto mensal da empresa

somaSalario :: Int -> Float
somaSalario s
    | s == 1 = salario 1
    | otherwise = salario s + somaSalario (s-1)

totalSalarios :: Float
totalSalarios = somaSalario 10


-- Funcao qtde salarios maiores que n

contMaior :: Float -> Int -> Int
contMaior valor i
    | i == 0 = 0
    | valor < salario i = 1 + contMaior valor (i-1)
    | otherwise = contMaior valor (i-1)

qtdMaior :: Float -> Int
qtdMaior valor = contMaior valor 10


-- Receber um determinado valor n e informar qual salário mais se aproxima deste valor.

aux :: Float -> Int -> Float -> Float
aux prox k n
    | k == 0 = prox
    | salario k > prox && salario k < n = aux (salario k) (k-1) n 
    | otherwise = aux prox (k-1) n

compara :: Float -> Int -> Float
compara valor r
    | valor > salario r =  aux (salario r) (r-1) valor 
    | otherwise = compara valor (r-1)

valorProx :: Float -> Float
valorProx n = compara n 10


-- Previdencia social
somaSalarioP :: Int -> Float
somaSalarioP id
    | id == 0 = 0
    | salario id < 1100.00 = salario id + somaSalarioP (id-1)
    | salario id > 1100.01 && salario id < 2203.48 = salario id - (salario id * 0.09) +  somaSalarioP (id-1)
    | salario id > 2203.49 && salario id < 3305.22 = salario id - (salario id * 0.12) +  somaSalarioP (id-1)
    | salario id > 3305.23 && salario id < 6433.57 = salario id - (salario id * 0.14) +  somaSalarioP (id-1)
    | salario id > 6433.57 = salario id - (salario id * 0.22) +  somaSalarioP (id-1)

prevSocial ::  Float
prevSocial = somaSalarioP 10
    