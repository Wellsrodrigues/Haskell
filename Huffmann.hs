
module Huffumann where
import Data.List
import PrintTree


------------------------------- TRATAMENTO DE PALAVRA --------------------------------------------

qtd :: Char -> String -> Int
qtd c word = sum [1 |  a <- word, c == a]

order :: [(Char, Int)] -> [(Char, Int)]
order [] = []
order ((a,b):l) = order [(x,y) | (x,y) <- l, y > b] ++ [(a,b)] ++ order[(x,y) | (x,y) <-l, y <= b]

freq :: String -> [(Char, Int)]
freq word = order[(a, qtd a word) | a <- nub word]

table :: [(Char, Int)] -> IO()
table [] = putStrLn " "
table ((a,b):ls) = do
                             putStrLn ("Caracter: " ++ show(a) ++ "-> freq: " ++ show(b))
                             table ls


------------------------------- TRATAMENTO DA ARVORE --------------------------------------------

-- data Tree a = Null | Node (Tree a) a (Tree a) 
--         deriving(Eq, Ord, Show)

createNode :: [(Char, Int)] -> Tree (Char, Int)
createNode ((a,x):(b,y):[]) = Node (Node Null (a,x) Null) ('+',x+y) (Node Null (b,y) Null)
createNode (a:ls) = createNode ls

valueNode :: Tree(Char, Int) -> (Char, Int)
valueNode (Node left node rigth) = node

conectNode :: Tree(Char, Int) -> Tree(Char, Int) -> [(Char, Int)] -> Tree(Char, Int)
conectNode tree1@(Node esq node dir) tree@(Node l no r) list = do
                                                if fst(last list) == '+'
                                                then Node esq node tree
                                                else do
                                                        if fst(list !! ((length list)-2)) == '+' then Node tree node dir
                                                        else Node tree1 ('_',0) tree
fstNode :: [(Char, Int)] -> Tree(Char, Int)
fstNode list = do
        let node = createNode list
        let value = valueNode node
        createTree node (updateList list value)


createTree :: Tree(Char, Int) -> [(Char, Int)] -> Tree(Char, Int)
createTree tree@(Node left@(Node l (a,b) r) node rigth@(Node ll (c,d) rg)) list = do
        if (length list > 1)
        then do
                if (valueNode tree == ('_', 0))
                then createTree newTree newList
                else do
                        let newNode = createNode list
                        let value = valueNode newNode
                        let treeBinary = conectNode newNode tree list
                        createTree treeBinary (updateList list value)
        else tree
                where
                newTree = (Node left ('+',b+d) rigth)
                newList = updateList list ('+',b+d)


pertenceLeft :: Tree(Char, Int) -> Char -> Bool
pertenceLeft tree@(Node left node rigth) smb = do
                let noL = valueNode left
                let noR = valueNode rigth
                if (left /= Null)
                then do
                        if(fst noL == smb)
                        then True
                        else do
                                if (rigth /= Null)
                                then do
                                        if (fst noR == smb)
                                        then True
                                        else pertenceLeft left smb
                                else False
                else False

searchL :: Tree(Char, Int) -> Char -> String
searchL tree@(Node left node rigth) smb = do
                let noL = valueNode left
                let noR = valueNode rigth
                if (left /= Null)
                then do
                        if(fst noL == smb)
                        then "0"
                        else do
                                if (rigth /= Null)
                                then do
                                        if (fst noR == smb)
                                        then "1"
                                        else do
                                                if (pertenceLeft tree smb) then "0" ++ searchR left smb else "1" ++ searchR rigth smb
                                else ""
                else ""

searchR :: Tree(Char, Int) -> Char -> String
searchR tree@(Node left node rigth) smb = do
        let noL = valueNode left
        let noR = valueNode rigth
        if (rigth /= Null)
        then do
                if(fst noR == smb)
                then "1"
                else do
                        if (left /= Null)
                        then do
                                if (fst noL == smb)
                                then "0"
                                else do
                                        if (pertenceLeft tree smb) then "0" ++ searchR left smb else "1" ++ searchR rigth smb
                        else ""
        else ""


searchTree :: Tree(Char, Int) -> Char -> String
searchTree tree c= do
        if (pertenceLeft tree c) then searchL tree c else searchR tree c

sequenceBinary :: Tree(Char, Int) -> String -> String
sequenceBinary tree [] = ""
sequenceBinary tree (a:ls) = searchTree tree a ++ sequenceBinary tree ls

tableBinary :: Tree(Char, Int) -> String -> IO()
tableBinary _ [] = return()
tableBinary tree (a:ls) = do
        putStrLn (show(a) ++ " -> " ++ (searchTree tree a))
        tableBinary tree ls

------------------------------- TRATAMENTO DE LISTA --------------------------------------------

remove :: [(Char, Int)] -> [(Char, Int)]
remove list = take (length list-2) list

delet :: [(Char, Int)] -> [(Char, Int)]
delet list = take (length list-1) list

add :: [(Char, Int)] -> (Char, Int) -> [(Char, Int)]
add list new = list ++ [new]

updateList :: [(Char, Int)] -> (Char, Int) -> [(Char, Int)]
updateList list newItem = order(add (remove list) newItem)

------------------------------- HUFFMANN--------------------------------------------

huffman :: IO ()
huffman = do
        putStrLn "----------------------------------------------------------------"
        putStrLn "\t\t\tALGORITMO DE HUFFMAN"
        putStrLn "----------------------------------------------------------------"
        putStr "Insira uma palavra: "
        word <- getLine
        let list = freq word
        putStrLn "----------------------------------------------------------------"
        putStrLn "Tabela de frequencia:\n"
        table list
        putStrLn "----------------------------------------------------------------"
        let treeB = fstNode list
        putStrLn ""
        putStrLn ("Árvore Binária: " ++ "\n")
        printt treeB
        putStrLn ""
        putStrLn "----------------------------------------------------------------"
        putStrLn ("Sequencia Binária: " ++ sequenceBinary treeB word)
        putStrLn "----------------------------------------------------------------"
        putStrLn ""
        putStrLn ("Tabela Binária: " ++ "\n")
        tableBinary treeB word
        putStrLn ""
        putStrLn "----------------------------------------------------------------"
        return()

------------------------------------------------------------------------------------------------------

av = freq "amorcd"

a = listNode av

rz = root a

-- t = tree rz (update a)
-- tree = orderT a

searchNode :: [Tree(Char, Int)] -> Tree(Char, Int) -> Tree(Char, Int)
searchNode (a:ls) x = if valueNode x == valueNode a then a else searchNode ls x

root :: [Tree(Char, Int)] -> Tree(Char, Int)
root ((Node l no r):nodeL:nodeR:ls) =  Node nodeL no nodeR

update ::  [Tree(Char, Int)] -> [Tree(Char, Int)]
update tree = drop 3 tree

k = take 1 a


        



listNode :: [(Char, Int)] -> [Tree(Char, Int)]
listNode [] = []
listNode (a:[]) = []
listNode list =  listNode (updateList list (valueNode (createNode list))) ++ [createNode list]

shw :: [Tree(Char, Int)] -> IO()
shw [] = return()
shw (a:ls) = do
        print a
        shw ls
