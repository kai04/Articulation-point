data Tree a b = Node a (Tree a b) (Tree a b) | Leaf b
	deriving Show

instance Functor (Tree a) where
	fmap f (Leaf b) = Leaf (f b)
	fmap f (Node a xt yt) = Node a (fmap f xt) (fmap f yt)


instance Applicative (Tree a) where
	pure a = Leaf a
	(Leaf f) <*> (Leaf a) = Leaf (f a)
	(Leaf f) <*> (Node a xt yt)= Node a (f <$> xt) (f <$> yt)
	(Node a xt yt) <*> tree = Node a (xt<*>tree) (yt<*>tree)

t2= Node 10 (Leaf 4) (Node 20 (Leaf 5) (Leaf 6))

instance  Monad (Tree a) where
	return a = Leaf a
	m >>= k = k <*> m  