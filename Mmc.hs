import Mdc
import Primos


mmc :: Int -> Int -> Int
mmc x y = div (x*y) (mdc x y);



aux :: Int -> Int -> Int -> Int
aux x y p
    | x == 1 && y == 1 = 1
    | mod x p == 0 && mod y p == 0 && primo p = p * aux (div x p) (div y p) p 
    | mod x p == 0 && mod y p /= 0 && primo p = p * aux (div x p) y p
    | mod x p /= 0 && mod y p == 0 && primo p = p * aux x (div y p) p
    | otherwise = aux x y (p+1)

mmc2 :: Int -> Int -> Int
mmc2 x y = aux x y 2
