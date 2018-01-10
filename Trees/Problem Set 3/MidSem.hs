--List elements are in leaf positions and every node contains number of list elements underneath it.
import Debug.Trace

data Tree a = Leaf Int a | Node Int (Tree a) (Tree a)
 	deriving Show

tree = Node 5 
			(Node 2 (Leaf 1 3)  (Leaf 1 4))
			(Node 3 (Leaf 1 5) (Node 2 (Leaf 1 2) (Leaf 1 9) ))

createTree (x:[]) = Leaf 1 x 
createTree (x:xs) = let l = length (x:xs) 
                        l1=take (div l 2) (x:xs)
                        l2=drop (div l 2) (x:xs)
                        t1 = createTree l1
                        t2 = createTree l2
					in Node l t1 t2


search (Leaf _ a) _ =  a
search (Node x xt yt) n = let l = getlength xt
                              getlength (Node a _ _) = a 
                              getlength (Leaf 1 _) = 1
                          in (if n>l then (search yt (n-l)) else (search xt n))				


delete (Node 2 xt yt) n = if (n==1) then yt else xt
delete (Node 3 xt yt) n = if (n==1) then yt else (Node 2 xt (delete yt (n-1)))
delete (Node x xt yt) n = let l = div x 2
                          in (if n>l then (Node (x-1) xt (delete yt (n-l))) else (Node (x-1) (delete xt n) yt))