---------------Just Wooooww-----------------------------------------------------------------------

data Htree a = Null | Fork a (Htree a) (Htree a)
 deriving Show

levels :: [a] -> [[a]]
levels l = levels' l 1 
levels' [] _ = []
levels' (x:xs) pow = takeElem pow (x:xs) : levels' (remove (x:xs) pow) (2*pow)
takeElem 0 _ = []
takeElem pow (x:xs) = x:takeElem (pow-1) xs
remove l 0 = l
remove (x:xs) n = remove xs (n-1)


mktree :: [a] -> [Htree a]
mktree l = let l' = levels l
		   in  foldr addLayer [Null] l'

addLayer (x:xs) [Null] = Fork x Null Null : addLayer xs [Null]
addLayer [] _ = []
addLayer (x:xs) (y1:y2:ys) = Fork x y1 y2  : addLayer xs ys 


instance Functor Htree where
	fmap f Null = Null 
	fmap f (Fork a xt yt) = Fork (f a) (fmap f xt) (fmap f yt) 