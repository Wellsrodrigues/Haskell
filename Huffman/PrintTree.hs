
--------------------------------------- CÓDIGO ALHEIO --------------------------------------

module PrintTree where
import Data.List

data Tree a = Null | Node (Tree a) a (Tree a) 
        deriving(Eq, Ord, Show)

trr = Node Null "d" (Node (Node Null "a" Null) "b" (Node Null "g" Null)):: Tree String

data ParentDir = PLeft | PRight | NoParent deriving (Show,Eq)
type ParentPos = Int
type Level = Int

dline = '|'
factor = 4

m c1 c2 = if c1 == dline then c1 else c2
zipWith' f xs [] = xs
zipWith' f [] xs = xs
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

build_line pd a pp level = foldl (zipWith' m) "" (((++"|").(flip replicate ' ') <$> (factor*) <$> pp)++[(replicate (factor*level) ' ')++cn++show a])
                           where cn = case pd of PLeft -> "└──"
                                                 PRight -> "┌──"
                                                 NoParent -> "───"

tprint :: Show a => ParentDir -> [ParentPos] -> Level -> Tree a -> [String]
tprint _ _ _ Null = []
tprint pd pp level (Node a l r) = tprint PRight new_pp_r (level+1) r ++
                                  [build_line pd l pp level] ++
                                  tprint PLeft new_pp_l (level+1) a
                                  where new_pp_r = case pd of PRight -> pp
                                                              PLeft -> pp++[level]
                                                              NoParent -> pp
                                        new_pp_l = case pd of PRight -> pp++[level]
                                                              PLeft -> pp
                                                              NoParent -> pp

printt t = putStr $ (intercalate "\r\n" (tprint NoParent [] 0 t))++"\r\n"
