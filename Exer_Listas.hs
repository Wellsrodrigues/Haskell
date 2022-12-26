module Exer_Listas where
import Fibonnacci


-- Questao 01

impares :: [Int]
impares = [x | x <- [1, 3 .. 100]]


pares :: [Int]
pares = [n | n <- [10, 12 .. 100]]


nImpares :: Int -> [Int]
nImpares 0 = []
nImpares n = [f | f <- [1, 3 .. n]]


multiplos :: Int -> [Int]
multiplos 0 = []
multiplos k = [z | z <- [1 .. k], mod z 3 == 0, mod z 5 == 0]


tuplaQuad :: Int -> [(Int, Int)]
tuplaQuad 0 = []
tuplaQuad n = [(a,a*a) | a <- [1 .. n]]


indiceMat :: [(Int, Int)]
indiceMat = [(lin,col) | lin <- [0 .. 2], col <- [0 .. 3]]


indiceMat2 :: Int -> Int -> [(Int, Int)]
indiceMat2 m n = [(r, s) | r <- [0 .. (m-1)], s <- [0 .. (n-1)]]



-- Questao 02

listaFibonacci :: Int->[Int]
listaFibonacci n = [fibonacci x | x <-[1 .. n]]



-- Questao 03

binHex :: String -> String
binHex "0000" = "0"
binHex "0001" = "1"
binHex "0010" = "2"
binHex "0011" = "3"
binHex "0100" = "4"
binHex "0101" = "5"
binHex "0110" = "6"
binHex "0111" = "7"
binHex "1000" = "8"
binHex "1001" = "9"
binHex "1010" = "A"
binHex "1011" = "B"
binHex "1100" = "C"
binHex "1101" = "D"
binHex "1110" = "E"
binHex "1111" = "F"


convt :: [Char] -> [Char]
convt [] = []
convt seq
    | mod (length seq) 4 == 0  = binHex (take 4 seq) ++ convt (drop 4 seq)
    | otherwise = convt (replicate 1 '0' ++ seq)



-- Questao 04

moverDisco :: Char -> Char -> [String]
moverDisco x y = ["mover disco torre " ++ (show x) ++ " para torre " ++ (show y)]

auxx :: Int -> Char -> Char -> Char -> [String]
auxx numDisco torre1 torre2 torre3
    | numDisco == 1 = moverDisco torre1 torre3
    | otherwise = auxx (numDisco-1) torre1 torre3 torre2 ++  moverDisco torre1 torre3 ++ auxx (numDisco-1) torre3 torre2 torre1

torreHanoi :: Int  -> [String]
torreHanoi numDisco = auxx numDisco 'A' 'B' 'C'

-----------------------------------------------------------

hanoi :: Int -> Int -> Int -> Int -> [String]
hanoi 0 _ _ _ = []
hanoi n origem aux destino
    | n ==  1 = [show (origem) ++ "->" ++show (destino)]
    | otherwise = hanoi (n-1) origem destino aux ++ [show (origem) ++ "->" ++show (destino)] ++ hanoi (n-1) aux origem destino