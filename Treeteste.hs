module Treeteste where
import PrintTree
import Huffumann (updateList)

fstNode :: [(Char, Int)] -> IO()
fstNode list = do
        let node = createNode list
        let value = valueNode node
        createTree node (updateList list value)


createTree :: Tree(Char, Int) -> [(Char, Int)] -> IO()
createTree tree@(Node left@(Node l (a,b) r) node rigth@(Node ll (c,d) rg)) list = do
        print list
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
                newList = updateList (delet list) ('+',b+d)


arv= (freq "wellison")