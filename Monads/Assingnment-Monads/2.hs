import Prelude hiding (Maybe,Just,Nothing)
import MCPrelude

data Maybe a = Just a | Nothing
instance Show a => Show (Maybe a) where
	show Nothing = "Nothing"
	show (Just a) = "Just "++show a 

headMay :: [a] -> Maybe a
headMay [] =  Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a,b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay a ((x1,x2):xs) | x1 == a = Just x2
						 | otherwise = lookupMay a xs	 

divMay :: (Eq a ,Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay a b = Just (a / b)

maximunMay :: Ord a => [a] -> Maybe a
maximunMay [] = Nothing
maximunMay l = Just (maximum l)

minimunMay :: Ord a => [a] -> Maybe a
minimunMay [] = Nothing
minimunMay l = Just (minimum l)


-----------------------------------------------------------------------------------------------
queryGreek :: GreekData -> String -> Maybe Double
queryGreek l s =  --let xs = lookupMay s l
				    case (lookupMay s l) of 
				  	    Nothing -> Nothing
				  	    Just l1 -> case tailMay l1 of
				  	    		     Nothing -> Nothing
				  	    		     Just l2 -> case maximunMay l2 of
				  	    			 				Nothing -> Nothing
				  	    			 	  			Just m -> case headMay l1 of 
				  	    			 	  			 	  		Nothing -> Nothing
				  	    			 	  			 	  		Just h -> divMay (fromIntegral m) (fromIntegral h)
			
-------------------------------------------------------------------------------------------------------
chain :: (a->Maybe b) -> Maybe a -> Maybe b
chain f Nothing = Nothing
chain f (Just a) = f a 

link :: Maybe a -> (a->Maybe b) -> Maybe b
link a f = chain f a

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 l s = let l1 = lookupMay s l 
                      t = link l1 (tailMay)
                      m = link t (maximunMay)
                      h = link l1 (headMay)
                      term1 = link m (\term -> Just (fromIntegral term))
                      term2 = link h (\term -> Just (fromIntegral term))
                  in  link term2 (\t2 -> link term1 (\t1 -> (divMay t1) t2))
                 
-----------------------------------------------------------------------------------------------------------------------

mkMaybe :: a -> Maybe a
mkMaybe a = Just a

addSalaries ::  [(String,Integer)] -> String -> String -> Maybe Integer
addSalaries list s1 s2 = case (lookupMay s1 list) of
							Nothing -> Nothing
							Just i1 -> case (lookupMay s2 list) of
										Nothing -> Nothing
										Just i2 -> Just (i1 + i2)

yLink :: (a->b->c) -> Maybe a -> Maybe b -> Maybe c
yLink f s1 s2 = link s1 (\a -> link s2 (\b -> mkMaybe(f a b)))
						     

addSalaries2 ::  [(String,Integer)] -> String -> String -> Maybe Integer
addSalaries2 list s1 s2 = yLink (+) (lookupMay s1 list) (lookupMay s2 list)


----------------------------------------------------------------------------------------------------------------------------

tailProd :: (Num a) => [a] -> Maybe a
tailProd list = case tailMay list of
				  Nothing -> Nothing
				  Just a -> Just (product a)

tailSum :: (Num a) => [a] -> Maybe a
tailSum list = case tailMay list of
				  Nothing -> Nothing
				  Just a -> Just (sum a)


transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f list = link list (\l -> mkMaybe (f l)) 

tailProd2 :: (Num a) => [a] -> Maybe a
tailProd2 list = transMaybe (product) (tailMay list)


tailSum2 :: (Num a) => [a] -> Maybe a
tailSum2 list = transMaybe (sum) (tailMay list)

tailMax :: (Ord a) => [a] -> Maybe (Maybe a)
tailMax list = transMaybe (maximunMay) (tailMay list)

tailMin :: (Ord a) => [a] -> Maybe (Maybe a)
tailMin list = transMaybe (minimunMay) (tailMay list)


combine ::  Maybe (Maybe a) -> Maybe a 
combine (Just (Just a)) = Just a
combine (Just Nothing) = Nothing
				   
tailMax2 :: (Ord a) => [a] -> Maybe  a
tailMax2 list = combine (transMaybe (maximunMay) (tailMay list))

tailMin2 :: (Ord a) => [a] -> Maybe a
tailMin2 list = combine (transMaybe (minimunMay) (tailMay list))
