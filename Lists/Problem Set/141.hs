summands 1 = [[1]]
summands n = summands' 1 n
 where  summands' i 1 = [[i]]
	summands' i n' = [i:x|x<-summands (n'-1)]++summands' (i+1) (n'-1)
