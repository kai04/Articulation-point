--wordbuilding l = [add (delete x l) [x]|x<-l]
--	where add  [] _ = []
--	      add  (x:xs) list | (head.head) list == last x = let x:list --
--							      in add xs list    --
--			       | otherwise = let list 
--					     in add xs



wordbuilding [x] = [[x]]
wordbuilding dict = concatMap f (dict) 
  where f word | null (validPlays word) = [[word]] 
               | otherwise = map (word:) (validPlays word) 
	validPlays word = filter (g (last word)) (wordbuilding (delete word dict))
	g c (x:xs) = c == head x

	delete e [] = []
	delete e (x:xs) | e==x = xs
			| otherwise = x : delete e xs 
