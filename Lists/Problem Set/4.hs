reverse (x:xs) = foldl f [] (x:xs)
	where f = (flip(:))
reverse1 (x:xs) = foldr (:) [] (x:xs)