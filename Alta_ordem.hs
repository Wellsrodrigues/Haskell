module Alta_ordem where

number :: [Int]
number = [1..3]

--------------------------------------

double :: [Int] -> [Int]
double = map (^2)

--------------------------------------

soma :: [Int] -> Int
soma lst = sum (double lst)

--------------------------------------

pos :: Int -> Bool
pos n = n>=0

teste :: [Int] ->[Bool]
teste = map pos
----------------------------------------

raiz = sqrt . abs


