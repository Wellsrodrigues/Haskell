module IO where

--SAIDA PADRAO:

char :: Char -> IO()
char ch = putChar ch

char2 :: IO()
char2 = putChar 'c'



string :: String -> IO()
string txt = putStr txt

string2 :: IO()
string2 = putStr "Uma string"


quebraLinha :: String -> IO()
quebraLinha str = putStrLn str

quebraLinha2 ::  IO()
quebraLinha2 = print("Quebra de linha" ++ quebraLinha2)

-- Show converte qualquer tipo em String
shw :: IO()
shw = putStrLn(show True)



--print :: Show a => a -> IO () imprime qualquer tipo como string
--print x = putStrLn (show x)

-- Varias funcoes IO em uma funcao
main ::  IO()
main = do
    print "String"
    print 123
    print True


text :: IO()
text = do
    putChar 'O'
    putChar 'i'
    putChar ','
    putStr "Como vai?"
    putStrLn "Tudo bem?"


-- ENTRADA PADRAO

input :: IO()
input = do
    --a <- getChar
    --print a
    --t <- getLine 
    --print t
    --w <- getContents
    --print w
    x <- getLine
    let t = x :: Double --type casting
    print t