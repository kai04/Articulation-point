--pascal :: Num t => [[t]]
--pascal = [1] : [x|x<-zipWith (+) ([0]++tail pascal) (tail pascal ++ [0])]
--pascal = iterate (\row -> zipWith (+) ([0]++row) (row++[0])) [1] 
pascal = iterate f [1]
  where f x = zipWith (+) ([0]++x)  (x++[0]) 
--add [] []= []
--add (a:as) (b:bs) = (a+b) : add as bs


--fib = iterate (\row->)
