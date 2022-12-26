module Fibonnacci where
import Primos (primo)

fibonacci :: Int -> Int
fibonacci n
    |n == 1 = 0
    |n == 2 = 1
    |otherwise = fibonacci(n-1) + fibonacci(n-2)


aux  :: Int -> Int -> Int
aux cont fib
    |cont == 0 = fibonacci (fib-1)
    |primo (fibonacci fib) = aux (cont-1) (fib+1)
    |otherwise = aux cont (fib+1)
    

nPrimoFib :: Int -> Int
nPrimoFib 1 = 2
nPrimoFib n = aux n 2