white = 1 
black = 0 
fewest_moves [] = 0
fewest_moves (1:xs) = min (1+fewest_moves xs) (1+fewest_moves (delete xs 1))
fewest_moves (0:xs) = min (1+fewest_moves xs) (1+fewest_moves (delete xs 3))
delete [] _ = []
delete (x:xs) 0 = (x:xs)
delete (x:xs) n = delete xs (n-1)		
