module Tree where

-------------------------------------------------------------

data Tree a = Null | Node (Tree a) a (Tree a)
        deriving(Eq, Ord, Show)

-------------------------------------------------------------

search :: (Eq a, Ord a) => Tree a -> a -> Tree a
search Null _ = Null
search (Node esq node dir) value
    | value == node = Node Null node Null
    | value < node = search esq value
    | otherwise = search dir value


-------------------------------------------------------------
arv = Node ((Node (Node Null 25 Null) 30 (Node (Node Null 38 Null) 45 Null))) 50 ((Node (Node Null 70 Null) 90 (Node Null 150 Null)))

-------------------------------------------------------------

create :: (Ord a) => [a] -> Tree a
create [] = Null
create (a:ls) = aux (Node Null a Null) ls
         where
        aux tree [] = tree
        aux tree (a:ls) = aux (inserir tree a) ls

-------------------------------------------------------------

inserir :: (Ord t) => Tree t -> t -> Tree t
inserir Null newNode = Node Null newNode Null
inserir (Node esq node dir) newNode 
        | newNode > node = Node esq node (inserir dir newNode)
        | newNode < node = Node (inserir esq newNode) node dir
        | otherwise = Node esq node dir

-------------------------------------------------------------

remove :: (Ord t) => Tree t -> t -> Tree t 
remove Null delNode = Null
remove (Node esq node dir) delNode
        | delNode == node = check dir esq
        | delNode > node = Node esq node (remove dir delNode)
        | otherwise = Node (remove esq delNode) node dir
        where
        check Null esq = esq
        check (Node e v d) esq = Node (check e esq) v d

-------------------------------------------------------------

order :: Tree a -> [a]
order Null = []
order (Node esq node dir) = order esq ++ [node] ++ order dir

inOrder :: (Show a) => Tree a -> IO()
inOrder Null = return ()
inOrder (Node esq node dir) = do
  inOrder esq 
  print node
  inOrder dir

-- show :: Tree a -> IO()
-- show Null = error "Vazia"
-- show arv = printt arv

