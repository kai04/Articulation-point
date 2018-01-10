fewest_moves :: [Int] -> Int

--fewest_moves [] = 0
fewest_moves (x:xs) | x == 1 && length xs == 1 = 1+fewest_moves xs
					| x == 1 && length xs > 1  = let choice1 = (1+fewest_moves xs)
					                                 choice2=(1+fewest_moves (tail xs))
							                    in if choice1 < choice2
							   		                  then choice1
							   	                   	  else choice2
					| x == 0 && length xs <=3 && length xs>1  = 1+fewest_moves xs		   	                   	  
					| x == 0 && length xs > 3 = let choice1=(1+fewest_moves xs)  
					                                choice2=(1+fewest_moves (tail(tail(tail xs))))
							      			    in if choice1 < choice2
							                 		    then choice1
							   		                    else choice2
					| otherwise = 0





