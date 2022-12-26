
module Prova2 where

import Data.Ord
import Data.List
--------------------------------------- 01 -------------------------------------------

qtde :: Eq a => a -> [a] -> Int
qtde _ [] = 0
qtde elem (a:ls)
    | elem == a = 1 + qtde elem ls
    | otherwise = 0
  

lossless :: String -> String
lossless [] = ""
lossless (a:ls) 
    | qtd == 1 = [a] ++ lossless del1
    | otherwise = [a] ++ show(qtd) ++ lossless resto
    where
    del1 = drop 1 (a:ls)
    qtd = qtde a (a:ls)
    resto = drop qtd (a:ls)


--------------------------------------- 02 ------------------------------------------

sequencia :: Eq a => [a] -> [(a, Int)]
sequencia [] = []
sequencia (a:ls) = [(a,qtd)] ++ sequencia resto
    where 
    qtd = qtde a (a:ls)
    resto = drop qtd (a:ls)

    
search :: Eq a => a -> [(a, Int)] -> [(a, Int)]
search simb lista = [ (a,b) | (a,b) <- lista, simb == a]

ordena :: Eq a => [a] -> [(a, Int)] -> [(a, Int)]
ordena [] _ = []
ordena (p:w) base = search p base ++ ordena w base


less :: Eq a => [a] -> [(a, Int)]
less lista = ordena (nub lista) (sequencia lista)
