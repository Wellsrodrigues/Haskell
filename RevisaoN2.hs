module RevisaoN2 where

-- Alta ordem 

dobro ::  [Int]
dobro = map (*2) [1..10]


raizQua :: [Double]
raizQua = map sqrt [1..10]


pares ::  [Int]
pares = filter even [1..10]

somaQuadPares :: Int
somaQuadPares = sum (map (^2) (filter even [1..5]))


-----------------------------------

triplo :: Int -> Int
triplo n = 3*n

prod :: (Int -> Int) -> Int -> Int
prod funcao n = funcao n

------------------------------------

-- Criar funcoes mistas

raiz :: Double -> Double
raiz = sqrt . abs




-- qtde :: Eq a => [a] -> Int
-- qtde (a:ls)
--      | a == head ls = 1 + qtde ls
--      | otherwise = 1

qtde :: Eq a => a -> [a] -> Int
qtde elem lista = sum [ 1 | x <- lista, x == elem]

    
contagem :: String -> String
contagem [] = ""
contagem (a:ls) 
    | qtde a (a:ls) == 1 = [a] ++ contagem (drop 1 ls)
    | otherwise = [a] ++ show(qtde a (a:ls)) ++ contagem (drop (qtde a (a:ls) -1) ls)

-- contagem :: String -> [(Char, String)]
-- contagem lst = [ (t, conv qtd) |  t <- lst, qtd <- [qtde t lst]]
