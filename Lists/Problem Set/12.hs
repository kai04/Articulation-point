ssm [] = []
ssm (x:xs) = x:ssm' xs x
	--longest (x : ssm' xs x) --(ssm xs)
	--where longest a b | length a >= length b = a
			 -- | otherwise = b 

ssm' [] _ = []
ssm' (x:xs) last | x > last = x:ssm' xs x 
                 | otherwise = ssm' xs last
