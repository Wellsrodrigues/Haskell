module Struct where


type Person = (String, String, Int)


pessoa :: Person
pessoa = ("Pedro", "99 9999-9999", 18)

name :: Person -> String
name (name, _, _) =  name

phone :: Person -> String
phone (_, number, _) = number

age :: Person -> Int
age (_, _, age) = age

showAll :: Person -> IO()
showAll (name, number, age) =  print (name, number, age)


---------------------------------------------------------------------------------------


type Dia = (Int)
type Mes = (Int)
type Ano = (Int)

type Data = (Dia, Mes, Ano)

date :: Data
date = (30, 11, 2020)

bissexto :: Data -> Bool
bissexto  (_, _, a)
    | mod a 4 == 0 && mod a 100 /= 0 || mod a 400 == 0 = True
    | otherwise = False


valida :: Data -> Bool
valida (d, m, a)
    | d < 1 || d > 31 = False
    | m < 1 || m > 12 = False
    | m == 2 || m == 4 && d > 30 = False
    | bissexto (d, m, a) && m == 2 && d == 29 = True
    | otherwise = True

    
---------------------------------------------------------------------------------------


type Hora = (Int, Int, Int)

hora :: Hora
hora = (24, 59, 59)

validaH :: Hora -> Bool
validaH (h, m, s)
    | h < 0 || h > 24 = False
    | m < 0 || m > 59 = False
    | s < 0 || s > 59 = False
    | otherwise = True




qtdSeg :: Hora -> Int
qtdSeg (h, m, s) = (h * 60 * 60) + (m * 60) + s




minHora :: Int -> Int
minHora segs = div segs 3600

mints :: Int -> Int
mints segs = div (segs - (minHora segs * 3600)) 60

segds :: Int -> Int
segds segs = segs - (minHora segs * 3600 + mints segs * 60)

convert :: Int -> Hora
convert segs = ( minHora segs, mints segs, segds segs)




diferenca :: Hora -> Hora -> Hora
diferenca (h1, m1, s1) (h2, m2, s2 )
    | h2 > h1 && m2 < m1 = ((h2-1) - h1, (m2+60) - m1, s2-s1)
    | h2 > h1 && m2 > m1 = (h2-h1, m2-m1, s2-s1)
    | h2 < h1 && m2 < m1 =  (23-(h1-h2), 60-(m1-m2), s2-s1)
    | h2 < h1 && m2 > m1 =  (23,(m2-m1), s2-s1)


