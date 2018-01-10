solve' [] [] (x:[]) [(x,1)] = 
solve (x:xs) (y:ys) (z:zs) = solve'  
