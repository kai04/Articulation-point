nQueen 0 = []
nQueen n = [board++pos|board<-nQueen,pos<-[1..8],safe pos board]
safe pos board = all p (zip [1..n-1] board)
	where p (n1,pos1) = safepos (n1,pos1) (n,pos)
		where safepos 
