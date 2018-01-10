import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] _ = [] 
allPairs (x:xs) l2 = generate x l2 ++ allPairs xs l2
 where  generate x [] = []
        generate x (y:ys) = (x,y) : generate x ys

---------------------------------------------------------------------------

data Card x y = Card x y
instance (Show x,Show y) => Show (Card x y) where 
   show (Card x y) =  (show x++ read (show y) :: String) 

allCards :: [a] -> [b] -> [Card a b]
allCards [] _ = []
allCards (x:xs) l2 = generate x l2 ++ allCards xs l2
	where generate x [] = []
	      generate x (y:ys) = (Card x y) : generate x ys

----------------------------------------------------------------------------------------

allCombs :: (a->b->c) -> [a] -> [b] -> [c]
allCombs f [] _ = []
allCombs f (x:xs) list2 = generate x list2 ++ allCombs f xs list2
	where generate x [] = [] 
	      generate x (y:ys) = f x y : generate x ys


allPairs2 list1 list2 = allCombs f list1 list2
	where f x y = (x,y)

allCards2 list1 list2 = allCombs f list1 list2
	where f x y = Card x y

------------------------------------------------------------------------------------------

allCombs3 :: (a->b->c->d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f [] _ _ = []
allCombs3 f (x:xs) list2 list3 = generate1 x list2 list3 ++ allCombs3 f xs list2 list3
    where generate1 x [] _ = []
     	  generate1 x (y:ys) list3 =  generate2 x y list3 ++ generate1 x ys list3  
     	  generate2 x y [] = []
          generate2 x y (z:zs) = f x y z : generate2 x y zs

---------------------------------------------------------------------------------------------

combSteps :: [a->b]->[a]->[b]	
combSteps [] _ = []
combSteps _ [] = []
combSteps (f1:fs) list = (map f1 list) ++ combSteps fs list


allCombs' :: (a->b->c) -> [a] -> [b] -> [c]
allCombs' f list1 list2 = combSteps (map f list1) (list2) 


allCombs3' :: (a->b->c->d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f list1 list2 list3 = combSteps (combSteps (map f list1) (list2)) list3

allCards2' list1 list2 = allCombs' f list1 list2
	where f x y = Card x y