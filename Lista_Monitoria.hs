import Data.Char

-- 01
pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False
pertence n (a:ls)
    | n == a = True
    | otherwise = pertence n ls

------------------------------------------------------

-- 02
maiorElem :: [Int] -> Int
maiorElem [] = 0
maiorElem (a:ls)
    | a >= maiorElem ls = a
    | a < maiorElem ls = maiorElem ls

------------------------------------------------------

-- 03
enesimo :: Int -> [Int] -> Int
enesimo _ [] = 0
enesimo n (a:ls)
    | n == 1 = a
    | otherwise = enesimo (n-1) ls

------------------------------------------------------

-- 04
removerEne :: Int -> [a] -> [a]
removerEne _ [] = []
removerEne 1 (a:ls) = ls
removerEne n (a:ls) = a : removerEne (n-1) ls

------------------------------------------------------

-- 05
reverso :: [a] -> [a]
reverso [] = []
reverso (a:ls) = reverso ls ++ [a]

palindromo :: Eq a => [a] -> Bool
palindromo normal
    | normal == reverso normal = True
    | otherwise = False

------------------------------------------------------

-- 06
naoRepete :: String -> String
naoRepete [] = []
naoRepete [a] = [a]
naoRepete (a:ls)
    | a /= head ls = a : naoRepete ls
    | a == head ls = naoRepete ls

------------------------------------------------------

-- 07
duplex :: [a] -> [a]
duplex [] = []
duplex (a:ls) = [a] ++ [a] ++ duplex ls

------------------------------------------------------

-- 08
firstWord :: String -> String
firstWord [] = []
firstWord (a:ls)
    | a == ',' = []
    | a == ' ' = []
    | otherwise = a : firstWord ls

------------------------------------------------------

-- 09
direita :: [Char] -> Int -> [Char]
direita [] _ = []
direita lista 0 = lista
direita (a:ls) n
    | n == 1 = [last ls] ++ [a] ++ removerEne (length ls) ls
    | otherwise = direita ([last ls] ++ [a] ++ removerEne (length ls) ls) (n-1)

-----------------------------------------------------

-- 10
comum :: [Int] -> [Int] -> [Int]
comum [] [] = []
comum list1 list2 = [ k | k <- list1, pertence k list2]

------------------------------------------------------

-- 11
maioresN :: [Int] -> Int -> [Int]
maioresN [] _ = []
maioresN (a:ls) n
    | a > n = a : maioresN ls n
    | otherwise = maioresN ls n

menoresN :: [Int] -> Int -> [Int]
menoresN [] _ = []
menoresN (a:ls) n
    | a <= n = a : menoresN ls n
    | otherwise = menoresN ls n

split :: [Int] -> Int -> ([Int], [Int])
split [] _ = ([],[])
split list p = (maioresN list p, menoresN list p)

------------------------------------------------------

-- 12
indice :: [a] -> Int -> [a]
indice [] _ = []
indice (a:ls) n
    | n == 1 = [a]
    | otherwise = indice ls (n-1)

intervalo :: [a] -> Int -> Int -> [a]
intervalo [] _ _ = []
intervalo lista m n
    | m <= n = indice lista m ++ intervalo lista (m+1) n
    | otherwise = []

------------------------------------------------------

-- 13
aux ::  Eq a =>  [a] -> [a]
aux  [] = []
aux (a:[]) = [a]
aux (a:sl)
    | a == head sl = [a] ++ aux sl
    | otherwise = [a]

-- aux lista = [ x | x <- lista, x == head lista]

sub_lista :: [Char] -> [String]
sub_lista [] = []
sub_lista (a:[]) = [[a]]
sub_lista lista = [aux lista] ++ sub_lista (drop (length (aux lista)) lista)

------------------------------------------------------

-- 14 ----Resolução com QuickSort

del :: Int -> [Int] -> [Int]
del _ [] = []
del r (a:ls)
    | a == r = ls
    | otherwise = [a] ++ del r ls

listaIdades :: [(Int, String)] -> [Int]
listaIdades [] = []
listaIdades lst = [x | (x,y) <- lst, x == fst(x,y)]

reg :: [(Int, String)] -> [(Int, String)]
reg [] = []
reg lista = axt lista (listaIdades lista)

axt :: [(Int, String)] -> [Int] -> [(Int, String)]
axt lista idades = [ (x,y) | (x,y) <- lista, x == maiorElem idades] ++ axt lista (del (maiorElem idades) idades)

------------------------------------------------------

-- 15
inter :: [Int] -> [Int] -> [Int]
inter [] _ = []
inter _ [] = []
inter (a1:z1) (a2:z2) = [a1] ++ [a2] ++ inter z1 z2

------------------------------------------------------

-- 16
cont :: [Int] -> Int
cont [] = 0
cont (a:[]) = 1
cont (a:zs)
    | a == head zs = 1 + cont zs
    | otherwise = 1

contt :: [Int] -> [[Int]]
contt [] = []
contt (a:[]) = [[1, a]]
contt lista
    | head lista == head (tail lista) = [[cont lista, head lista]] ++ contt (drop (cont lista) lista)
    | otherwise = [[1,head lista]]

------------------------------------------------------

-- 17
generic :: Eq a => [Int] -> [a] -> [a]
generic _ [] = []
generic [] lista = lista
generic pos base = base ++ [ g | g <- base, w <- pos, [g] == indice base w]

------------------------------------------------------

-- 18
meio :: [Int] -> ([Int], [Int])
meio [] = ([],[])
meio list = (take (div (length list) 2) list, drop (div (length list) 2) list)

------------------------------------------------------

-- 19
add :: [Int] -> Int -> [Int]
add [] n = [n]
add list n = list ++ [n]

------------------------------------------------------

-- 20
desconto :: [(Double, Int)] -> [Double]
desconto [] = []
desconto ((preco,idade): ls)
    | idade >= 60 = [0.6 * preco] ++ desconto ls
    | idade > 2 && idade <= 10 = [0.5 * preco] ++ desconto ls
    | idade > 0 && idade <= 2 = [0.1 * preco] ++ desconto ls

------------------------------------------------------

-- 21
imc :: Float -> Float -> Float
imc 0 _ = 0
imc _ 0 = -1
imc peso altura = peso / (altura * altura)


classifica :: [(Int, Float, Float)] -> [(Int, String)]
classifica [] = []
classifica ((cod, peso, altura): ls)
    | imc peso altura < 18.5 = [(cod, "Abaixo do peso")] ++ classifica ls
    | imc peso altura >= 18.5 && imc peso altura < 25.0 = [(cod, "Peso normal")] ++ classifica ls
    | imc peso altura >= 25.0 && imc peso altura < 30.0 = [(cod, "Excesso de peso")] ++ classifica ls
    | imc peso altura >= 30.5 = [(cod, "Obesidade")] ++ classifica ls

------------------------------------------------------

-- 22
divprop :: Integer -> [Integer]
divprop n = [ x | x <- [1..(n-1)], mod n x == 0]

------------------------------------------------------

-- 23
perfeitos :: Integer -> [Integer]
perfeitos p = [k | k <- [1..(p-1)], sum (divprop k) == k]

------------------------------------------------------

-- 24
pitagoricos :: Integer -> [(Integer, Integer, Integer)]
pitagoricos 0 = []
pitagoricos t = [ (a,b,c) | a <- [1..t], b <- [1..t], c <- [1..t], (a*a) + (b*b) == (c*c)]
------------------------------------------------------

-- 25
alfabeto :: String
alfabeto = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

indexx :: Char -> [Char] -> Int
indexx _ [] = 0
indexx 'Z' _ = -1
indexx tl cj
    | tl == head cj = 0
    | otherwise = 1 + indexx tl (tail cj)

cifrar :: Int -> String -> String
cifrar 0 txt = txt
cifrar n lista = [ if l /= ' ' then alfabeto !! ((indexx l alfabeto) + n) else ' ' | l <- lista]

------------------------------------------------------

-- 26
maiuscula :: String -> String
maiuscula txt = [ l | l <- txt, isUpper l]

minuscula :: String -> String
minuscula txt = [ l | l <- txt, isLower l]

algarismos :: String -> String
algarismos txt = [ p | p <- txt, pertence p "0123456789"]
    
password :: String -> Bool
password senha = and [length senha >= 8, maiuscula senha /= "", minuscula senha /= "", algarismos senha /= ""]

------------------------------------------------------

-- 27
iguals :: Char -> [Char] -> [Char]
iguals _ [] = []
iguals c lista = [ a | a <- lista, a /= c]

nubb :: String -> String
nubb [] = []
nubb (a:ls)
    | not(pertence a ls) = [a] ++ nubb ls
    | pertence a ls = [a] ++ nubb (iguals a ls)


------------------------------------------------------

-- 28
sperse :: Char -> String -> String
sperse _ [] = []
sperse _ (a:[]) = [a]
sperse sib (a:ls) = [a] ++ [sib] ++ sperse sib ls

------------------------------------------------------

-- 29
convert :: Int -> [Int]
convert 0 = [0]
convert 1 = [1]
convert d
    | mod d 2 == 0 = [0] ++ convert (div d 2)
    | otherwise = [1] ++ convert (div d 2)

forBits :: Int -> [Int]
forBits num = reverse (convert num)

------------------------------------------------------

-- 30
dec :: [Int] -> Int
dec lista = sum (conv lista ((length lista)-1))


conv :: [Int] -> Int -> [Int]
conv [] _ = []
conv lst 0 = lst
conv bits pos
    | head bits == 0 || head bits == 1 = [head bits * 2^pos] ++ conv (tail bits) (pos-1)
    | otherwise = []
