type Graph = [(Int,Int)]
type Node = Int
type Path = [Int]

makepath :: Node -> Graph -> Graph
makepath node (x:xs) = filter f (x:xs)
	where f (n1,n2) = node == n1 
