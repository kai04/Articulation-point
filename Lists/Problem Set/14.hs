import Data.List
get_num n = [1..n] :: [Int]
summands :: Int -> [[Int]]
summands n = [x|x<-subsequences (get_num n),sums x n]
	where sums a b | sum a == b = True
      			   | otherwise = False

summands1 n = summands1' n 1
summands1' n i = [] ++ summands1' n (i+1)
summands1' n n = [[n]] 
