
length (x:xs) = foldr f id (x:xs) 
	where   id = 0
		f a = (+1)
		      
sum (x:xs) = foldr f id (x:xs) 	
	where   id = 0
		f a b = a + b


--(x:xs) ++ ys = foldr (:) ys (x:xs)

--reverse (x:xs) = foldl f [] (x:xs)
--	where f= (flip(:))
--reverse (x:xs) = foldr f x xs
--	where f a b = (:) a b

--map f [] = []
map f (x:xs) = foldr ((:) . f) [] (x:xs)


--Working--filter p (x:xs) = foldr ((++) . f) [] (x:xs)
	--where f x | p x = [x]
--		  | otherwise = []

--working filter p (x:xs) = foldr f [] (x:xs)
--	where f x | p x = (x:)
--		  | otherwise = id
			 
filter f (x:xs) = foldr (\x -> if f x then (x:) else id) [] (x:xs)

take n (x:xs) = foldr f id (x:xs) n
	where f x g 0 = id 
	      f x g n = x:g (n-1)
	      id=const []	
 	  
