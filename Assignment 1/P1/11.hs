--type f a =f a -> Bool
--data Set = Set f
--insert :: Set -> Set
--insert a =  
--type Set a = a -> Bool

instance Show (a -> b) where
         show a= "funcion"

data Set a = Set (a->Bool) deriving (Show) 

empty :: Set f
empty = Set (\_ -> False)

comp :: Set f -> Set f   
comp (Set f) = Set (\x -> not(f x))
--insert :: Set -> a -> (a -> Bool)
--insert f a = f (a True)

insert :: Set f -> a -> Set f
insert (Set f) a = let f a=True
				   in  Set f


--member :: Set f -> a -> Bool
member (Set f) a = (f a)

union :: Set f -> Set f -> Set f
union (Set f1) (Set f2) = Set (\x -> (f1 x) || (f2 x))

intersection :: Set f -> Set f -> Set f
intersection (Set f1) (Set f2) = Set (\x -> (f1 x) && (f2 x))

difference :: Set f -> Set f -> Set f
difference (Set f1) (Set f2) = Set (\x -> (f1 x) && not(f2 x))