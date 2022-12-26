module Listas where

-- DECLARACAO

letras :: [Char]
letras = ['A', 'B', 'C'] 

pares :: [Int]
pares = [0, 2 .. 10]

nomes :: [String]
nomes = ["Ana", "Thiago", "Alan"]

primosGemeos :: [(Int, Int)]
primosGemeos = [(3, 5), (5, 7), (11, 13)]

tuplas :: [(String, Int, Char)]
tuplas = [("Joao", 18, 'M'), ("Maria", 21, 'F')]

listaTelefonica :: [[Int]]
listaTelefonica = [[559911112222], [118877773333]]



palavras :: [[Char]]
palavras = ["Ana", ['a','b','c'], "j"]


--OPERACOES

num :: [Int]
num = [1 .. 10]

dobro :: [Int]
dobro = [a*2 | a <- num]

triplo :: [Int]
triplo = [a*3 | a <- [1..5]]

headL :: [a] -> [a]
headL [] = []
headL (head:tail) =[head]

ult :: [Char] -> Char
ult (a:[]) = a
ult (a:ls) = ult ls

plt :: [Char] -> [Char]
plt (a:b:[]) = [a]
plt (a:ls) = plt ls

sumList :: [Int] -> Int
sumList [] = 0
sumList (a:x) = a + sumList x


vaziaL :: [a] -> Bool
vaziaL [] = True
vaziaL (head:tail) = False

tst :: [a] -> Bool
tst list
    | vaziaL list = True
    | otherwise = False

-- EXERCICIO

replica :: Char -> Int -> [Char]
replica simb 0 = []
replica simb n = simb : replica simb (n-1)


tamanho :: [Char] -> Int
tamanho [] = 0
tamanho (a:x) = 1 + tamanho x


recorte :: Int -> [a] -> [a]
recorte 0 _ = []
recorte _ [] = []
recorte n (a:x) = a : recorte (n-1) x

quatt :: Int -> [a] -> [a]
quatt 0 _ = []
quatt _ [] = []
quatt n (a:x) = a : quatt (n-1) x

remover :: [Char] -> [Char]
remover [] = []
remover (a:[]) = []
remover (a:z) = a : remover z


invert :: [Char] -> [Char]
invert [] = []
invert (a:x) = (invert x) ++ [a]


quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (a:x) = quickSort [y | y <- x, y <= a] ++ [a] ++ quickSort[y | y <- x, y > a]


divs :: Int -> [Int]
divs 0 = []
divs n = [x | x <- [1 .. (div n 2)], mod n x == 0]


perf :: Int -> [Int]
perf 0 = []
perf n = [x | x <- [1 .. n], sumList(divs x) == x]


concat :: [[Int]] -> [Int]
concat lista = [e | l <- lista, e <- l]

indexx :: [a] -> [Int]
indexx txt = [d | d <- [0..(length txt - 1)]]