wordbuilding [x] = [[x]]
wordbuilding dict = concatMap f dict
 where  f word | null (validWords word) = [[word]]
               | otherwise = map ((:)word) (validWords word)
        validWords word = filter (g word) (wordbuilding (delete word dict))
        g word1 word2 = last word1 == (head (head word2))
        delete word [] =[]
        delete word (x:xs) | word == x = xs
                           | otherwise = x:delete word xs

--isBeautiful i k = select1 i k 0 == select2 i k 0 
select1 i k m | (i < 10) = 1
			  | (m `mod` k) == 0 =  (i `mod` 10) + (10^m) * (select1 (i `div` 10) k (m+1)) 
			  | otherwise = select1 (i `div` 10) k m