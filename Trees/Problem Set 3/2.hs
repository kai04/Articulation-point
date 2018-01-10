data BTree a=Node (BTree a) (BTree a) | Leaf a

ht :: BTree Char
ht = Node (Leaf 'a')
		  (Node 
			(Node (Leaf 'b') (Node (Leaf 'c') (Leaf 'd'))) 
			(Node (Node (Leaf 'e') (Leaf 'f')) 
				  (Node (Leaf 'g') (Leaf 'h'))))

decode :: [Int] -> BTree Char -> [Char]
decode (x:xs) t = decode1 (x:xs) t
  where 
   decode1 [] (Leaf a) = [a]
   decode1 (x:xs) (Leaf a) = a : decode1 (x:xs) t 
   decode1 (0:xs) (Node l r) = decode1 xs l 
   decode1 (1:xs) (Node l r) = decode1 xs r



encode :: [Char] -> BTree Char -> [Int]
-- encode (x:xs) t = concat [[ans|ans<-f c (convert t)]|c<-(x:xs)]
 -- where f c ((c',l):ys) | c == c' = l
--	 	       | otherwise = f c ys	   
encode [] _ = []
encode (x:xs) t = let  l1 = convert1 t
                       l2 = f x l1
                       f = (\x ((c,l'):ys) -> if (x==c) then l' else f x ys)
				  in   (l2++encode xs t)
       





convert :: BTree Char -> [(Char,[Int])]
convert (Leaf c) = [(c,[])]
convert (Node xt yt) = insert 0 (convert xt) ++ insert 1 (convert yt) 
insert :: Int -> [(Char,[Int])] -> [(Char,[Int])]
insert i l = map f l
	where f (c,l') = (c,i:l')


convert1 t = convert' [] t
convert' l (Leaf c) = [(c,l)]
convert' l (Node xt yt) = convert' (l++[0]) xt ++ convert' (l++[1]) yt



