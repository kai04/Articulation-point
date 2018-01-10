data Tree a = Leaf | Node a (Tree a) (Tree a) 
 deriving Show

 
insert ::  Int -> Tree a -> Tree a
insert x (Node a xt yt) = 