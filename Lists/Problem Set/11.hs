import Debug.Trace
good_nums = [2..99] :: [Int]

good_factors p = [(a,b) | a<-good_nums,b<-good_nums,a*b==p,a<=b]
 
good_summands s = [(a,b) | a<-good_nums,b<-good_nums,a+b==s,a<=b]

singleton x | length x == 1 = True 
            | otherwise = False

fact1 (a,b) | singleton (good_factors (a*b)) = False
			| otherwise = True

fact2 (a,b) | singleton (good_summands (a+b)) = False
			| otherwise = True

fact3 (a,b) = let z = [(x*y) | (x,y) <- good_summands(a+b)] 
			  in fact3' z
fact3' []     = True
fact3' (x:xs) | singleton(good_factors x) = False
	          | otherwise = fact3' xs

fact4 (a,b)  | singleton([(x,y) | (x,y)<-good_factors (a*b), fact3 (x,y)]) = True
			 | otherwise = False

--fact5 (a,b) = let z = [(x*y) | (x,y) <- good_summands(a+b)] 
--			  in fact5' z
--fact5' [] = True
--fact5' (x:xs) =  let l =[(a,b)|(a,b)<-good_factors(x),fact3 (a,b)] 
--				 in if l == [] 
--				 	    then fact5' xs
--				        else False

fact5 (a,b) | singleton [(x,y) | (x,y) <- good_summands(a+b),fact4(x,y)] = True
			| otherwise = False
 