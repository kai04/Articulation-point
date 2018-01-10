errno l | wb l = 0
	| otherwise = find l 0
find (x:xs) count | count==0 && x==')' = find xs
		  | otherwise = x:find xs count



wb l = wb' l 0
wb' [] count | count == 0 = True
	     | otherwise = False 
wb' (x1:xs) (count) | count < 0 = False
                    | x1 == '('  = wb' xs (count+1) 
                    | x1 == ')'  = wb' xs (count-1)
	            
