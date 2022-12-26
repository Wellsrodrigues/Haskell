module InteiroPositivo where

inteiroPositivo :: Int -> Bool
inteiroPositivo n 
    |n >= 0 = True
    |otherwise = False