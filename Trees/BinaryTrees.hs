data Btree a = Leaf a | Fork (Btree a) (Btree a)

b1 :: Btree Int
b1 = Fork (Fork  
				(Leaf 1) (Fork (Leaf 2) (Leaf 3)))
		  ( Fork (Leaf 4) (Leaf 5))

size :: Btree a -> Int
size (Leaf _) = 1
size (Fork l r) = 1 + size l + size r

flatten :: Btree a -> [a]
flatten (Leaf a) = [a]
flatten (Fork l r) = flatten l ++ flatten r

data Stree a = Null | Node a (Stree a) (Stree a) 
	deriving Show

b2 = Node 1 
		(Node 2 Null (Node 4 Null Null))
		(Node 3 Null Null)

flattens Null = []
flattens (Node a l r) = flattens l ++ a ++ flattens r

insert :: (Ord a) => a ->Stree a -> Stree a
insert x Null = Node x Null Null
insert x t@(Node a l r)  | x==a = t
						 | x<a	= Node a (insert x l) r
						 | x>a  = Node a l (insert x r)