data RadixTree  = Leaf Color | Node Color RadixTree RadixTree
data Color = White|Black

insert :: BinaryString -> RadixTree -> RadixTree

insert [] (Leaf _) = Leaf White 
insert [] (Node _ xt yt) = Node White xt yt
insert (0:xs) (Leaf c) = Node c (insert xs (Leaf Black)) (Leaf Black) 
insert (1:xs) (Leaf c) =Node c (Leaf Black) (insert xs (Leaf Black))
