type Bird = Integer
type Pole = (Bird,Bird)

landLeft :: Bird -> Pole  -> Maybe Pole
landLeft n (left,right) | abs(left+n-right) < 4 = Just (left+n,right)
	      		| otherwise = Nothing 

landRight :: Bird -> Pole -> Maybe Pole
landRight n (left,right) | abs(left+n-right) < 4 = Just (left,right+n)
			 | otherwise = Nothing 

x -: f = f x
