import InteiroPositivo

divInteira :: Int -> Int -> Int
divInteira x y
  | inteiroPositivo x && inteiroPositivo y &&  y == 1 = x
  | inteiroPositivo x && inteiroPositivo y &&  x == y = 1
  | inteiroPositivo x && inteiroPositivo y &&  x < y = 0
  | inteiroPositivo x && inteiroPositivo y &&  x > y = div x y
  | otherwise = -1