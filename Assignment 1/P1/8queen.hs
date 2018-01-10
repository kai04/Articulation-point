--queen 1 = [[1],[2],[3],[4],[5],[6],[7],[8]]
queen 0 = [[]]
queen n = [board++[pos]|board <- queen (n-1),pos<-[1..8],safepos board pos]
	where safepos board pos =all p (zip [1..n-1] board)
			where p (n1,pos1) = safe n1 pos1 n pos
			      safe n1 pos1 n2 pos2 = n1/=n2 && pos1/=pos2 && abs(n2-n1)/=abs(pos2-pos1) 
				        			 