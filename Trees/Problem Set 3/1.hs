--dia NUll = 0
--dia (Node t1 a t2) |  

--foldtree f id Null =id
--foldtree f id (Node t1 a t2) = f a (foldtree f id t1) (foldtree f id t1)



data Tree a = Null | Node (Tree a) a (Tree a)

dia1 (Tree a) = dia (Tree a) 0 0 
dia Null = 0
dia (Node xt a yt) | (depth xt + depth yt) > dia xt
 
