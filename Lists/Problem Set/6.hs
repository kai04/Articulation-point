import Debug.Trace
--import Debug.Trace
--lcs [] y = []
--lcs (x:xs) (y:ys) | length(cs (x:xs) (y:ys)) >  length (lcs xs ys) = trace (show ((x:xs)++(y:ys))++"  then") (lcs xs ys)
		--			| otherwise = trace (show ((x:xs)++(y:ys)++"   else")) (cs (x:xs) (y:ys))
--cs a [] = []
--cs [] b = []
--cs (x:xs) (y:ys) | x==y = x : (cs xs ys)
 --                | length (x:xs) <= length (y:ys) = cs (x:xs) (ys)
 --                | otherwise =  cs (xs) (y:ys)

lcs _ []  = [] 
lcs [] _  = []
lcs (x:xs) (y:ys) | x==y = x:lcs xs ys
		  | otherwise = let lcs1=lcs (x:xs) ys 
	                            lcs2=lcs xs (y:ys) 
	          	        in if length lcs1 > length lcs2 
	      	                      then lcs1 
				      else lcs2 
