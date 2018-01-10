cprod [] = [[]]
cprod (xs:xss) = [x:y|x<-xs,y<-yss]
	where yss=cprod xss