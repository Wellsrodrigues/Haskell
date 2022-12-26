module Mercado_Sistema where
------------------------------------------------------------------------------------------

type Product = (String, Int, Float)

getName :: Product -> String
getName (name, _, _) = name

getQtd ::Product -> Int
getQtd (_, qtd, _) = qtd

getValue :: Product -> Float
getValue (_, _, value) = value

------------------------------------------------------------------------------------------

data Tree a = Null | Node (Tree Product) Product (Tree Product)
    deriving(Eq, Ord, Show)

------------------------------------------------------------------------------------------

produtos = createTree([("geleia", 6, 2.0), ("jengibre", 6, 3.0), ("doce", 9, 1.0),
                ("arroz", 5, 5.0), ("feijao", 10, 4.0), ("iodo", 5, 3.0),
                ("manteiga", 5, 2.0),  ("biscoito", 1, 3.0)])

            --          g
            --       /     \
            --     d         j
            --   /  \       /  \
            --  a    f    i     m
            --    \
            --      b

list :: [Product]
list = [("f", 2, 2), ("a", 2, 3), ("b", 1, 5), ("j", 4, 7), ("h", 4, 2), ("k", 1, 6)]

            --          f
            --        /    \
            --      a       j
            --       \     / \  
            --        b   h   k 

tree = createTree([("jaca", 6, 2.0), ("manga", 6, 3.0), ("goiaba", 9, 1.0),
            ("pera", 5, 5.0), ("ata", 10, 4.0), ("figo", 5, 3.0),
            ("uva", 5, 2.0)])


------------------------------------------------------------------------------------------

insert :: Tree Product -> Product -> Tree Product
insert Null prod = Node Null prod Null
insert (Node esq node  dir) prod = case compare (getName prod) (getName node) of
                                    EQ -> Node esq node dir
                                    LT ->  Node (insert esq prod) node dir
                                    GT -> Node esq node (insert dir prod)

------------------------------------------------------------------------------------------

createTree :: [Product] -> Tree Product
createTree [] = Null
createTree (a:ls) = aux (Node Null a Null) ls
        where
        aux tree [] = tree
        aux tree (a:ls) = aux (insert tree a) ls

------------------------------------------------------------------------------------------

order :: Tree Product -> Tree Product
order Null = Null
order (Node esq node dir) = Node (order esq) node (order dir)

showT :: Tree Product -> IO()
showT Null = return ()
showT (Node esq node dir) = do
    showT esq 
    print node 
    showT dir

------------------------------------------------------------------------------------------

remove ::  Tree Product -> String -> Tree Product
remove Null _ = Null
remove (Node esq node dir) delNode = case compare delNode (getName node) of
                                    EQ -> check dir esq
                                    LT -> Node (remove esq delNode) node dir
                                    GT -> Node esq node (remove dir delNode)
                                    where
                                    check Null esq = esq
                                    check (Node esq_ value dir_) esq = Node (check esq_ esq) value dir_


---------------------------------------- QUESTAO 01 --------------------------------------------------

-- função que utiliza a estrutura de uma árvore binária ordenada para buscar um
-- determinado produto pelo seu nome.


search :: String -> Tree Product -> Product
search _ Null = error "Sem produtos no estoque"
search name (Node left node right) = case compare name (getName node) of
                                    EQ -> node
                                    LT -> search name left
                                    GT -> search name right



---------------------------------------- QUESTAO 02 --------------------------------------------------

-- função que recebe um produto e atualiza suas informações na árvore ou adiciona
-- o produto na ordem correta caso ele ainda não tenha sido cadastrado.


estoque :: Tree Product -> String -> Bool
estoque Null _ = False
estoque (Node esq node dir) prod = case compare prod (getName node) of
                                    EQ -> True
                                    LT -> estoque esq prod
                                    GT -> estoque dir prod


update :: Tree Product -> Product -> Tree Product
update Null _ = Null
update tree@(Node esq node dir) newProd
    | not (verifica) = insert tree newProd
    | otherwise = alter
    where
    verifica = estoque tree (getName newProd)
    alter =  case compare (getName newProd) (getName node) of
                EQ -> Node esq newProd dir
                LT -> Node (update esq newProd) node dir
                GT -> Node esq node (update dir newProd)

---------------------------------------- QUESTAO 03 --------------------------------------------------

-- função que retorna a soma total de produtos do estoque (em R$), obtido
-- multiplicando-se o preço pela quantidade de todos os produtos armazenados.


cast :: Int -> Float
cast n = fromInteger (toInteger n)

total :: Tree Product -> Float
total Null = 0
total (Node esq node dir) = ((cast(getQtd node)) * (getValue node)) + total esq + total dir

receita :: Tree Product -> IO()
receita tree = print ("Receita: " ++ show (total tree) ++ " R$")



---------------------------------------- QUESTAO 04 --------------------------------------------------

-- Função que recebe o nome e quantidade de produtos a serem vendidos.
-- A quantidade de produtos deve ser subtraída do seu registro na árvore.
-- Deve remover da árvore os registros de produtos com estoque menor que 1


deficit :: Tree Product -> Tree Product
deficit Null = Null
deficit tree@(Node esq node dir) = if (getQtd node) < 1 then remove tree (getName node) else tree


venda :: String -> Int -> Tree Product -> Tree Product
venda _ _ Null = error "Sem produtos no estoque"
venda name qtd tree@(Node esq node dir) = case compare name (getName node) of
                                        EQ -> test
                                        LT -> Node (venda name qtd esq) node dir
                                        GT -> Node esq node (venda name qtd dir)
                                        where 
                                        change = update tree (getName node, (getQtd node)-qtd, getValue node)
                                        test = deficit change



---------------------------------------- FUNCOES --------------------------------------------------

menu :: IO()
menu = do
    putStrLn "\n------- MENU --------\n"
    putStrLn "Arvores: produto / tree"
    putStrLn "Lista: list"
    putStrLn "insert: Tree -> Product"
    putStrLn "createTree: [Product]"
    putStrLn "remove: Tree -> Nome"
    putStrLn "search: Nome -> Tree" 
    putStrLn "update: Tree -> Product"
    putStrLn "receita: Tree"
    putStrLn "venda: Nome -> Qtd -> Tree\n"                           

