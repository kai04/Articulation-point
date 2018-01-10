
hof a b c [] = b 
hof a b c (x:xs) = c ( a x ) (hof a b c xs)
 
map f (x:xs) = hof f [] (:) (x:xs)

foldr f id (x:xs) = hof nothing id f (x:xs)
nothing a = a

--sum l = Main.foldr g id l
--	where g  = (+) 
--	      id = 0

reverse (x:xs) = hof (:[]) [] (flip(++)) (x:xs)

takewhile f (x:xs) = hof nothing [] g (x:xs)
	where g a b | f x = a:b
		    | otherwise = []
