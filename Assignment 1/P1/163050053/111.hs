--Set :: (a -> Bool)
data Set a = Set(a -> Bool)


insert (Set f) elem = Set(\x-> (f x) || (x==elem))

empty = Set (\_ -> False)

member (Set f) a = f a

union (Set f1) (Set f2) = Set(\x-> f1 x || f2 x) 
