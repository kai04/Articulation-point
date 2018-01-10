import Debug.Trace
carmichael n = find n 563
find n i    | n==1 = (i-2)
            | isCarmichael i = find (n-1) (i+2) 
            | otherwise = find n (i+2)

isCarmichael num | prime num (num `quot` 2) = False
				 | otherwise = calculate num (num-1) 
	where prime n i | i==1 = True
				    | gcd n i /=1 =False
				    | otherwise = prime n (i-1) 
calculate number 1 = True
--calculate number i |  {-isRelativePrime number i-}gcd number i && i^(number-1) `mod` number == 1 = {-trace ("1 "++show i)-} calculate number (i-1)
--		    	   |  {-isRelativePrime number i-}gcd number i && i^(number-1) `mod` number /= 1 = {-trace ("2 "++show i)-} False
--			       |  otherwise = {-trace ("3 "++show i)-} calculate number (i-1)
calculate number i 
  | gcd number i /= 1 = calculate number (i-1)
  | i^(number-1) `mod` number /= 1 = False                      
  | otherwise = calculate number (i-1)
 
--isRelativePrime n 1 = trace("prime"++show n) True
--isRelativePrime n m	| gcd n m == 1 = True
--			      	| otherwise = False
	--where gcd a b | b==0 = a
--				  | otherwise = gcd b (a `mod` b)
--relativePrimes n = relativePrime n (n-1)
--relativePrime n m | m == 1 = putStrLn $m 
--				  |(n `mod` m) == 0 = do putStrLn $m 
--				                          relativePrime n (m-1) 
--	              | otherwise = relativePrime n (m-1)

