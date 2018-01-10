import Prelude hiding (Monad,return,Just,Nothing,Maybe,sequence)
import MCPrelude


--data Maybe a = Just a | Nothing
--instance Show a => Show (Maybe a) where
--	show Nothing = "Nothing"
--	show (Just a) = "Just "++show a 

---------------------------------------------------------------------------------
--generalB :: (a->b->c) -> Gen a -> Gen b -> Gen c
--yLink :: (a->b->c) -> Maybe a -> Maybe b -> Maybe c 
---generalBCommon :: (a->b->c) -> m a -> m b -> m c

--genTwo ::Gen a -> (a->Gen b)-> Gen b
--link :: Maybe a -> (a->Maybe b) -> Maybe b

--genTwoCommon :: m a -> (a-> m b) -> m b


----------------------------------------------------------------------------------------------
type Gen x = Seed -> (x,Seed)  -------type Synonyms

genTwo :: Gen a -> (a -> Gen b) -> Gen b   ---From 1.hs
genTwo f k = \s -> let (i,s') = f s
                    in (k i s')
mkGen :: a -> Gen a 						---From 1.hs
mkGen a = (\s->(a, s))

generalA :: (a->b) -> Gen a -> Gen b    	---From 1.hs
generalA f1 f2 seed = let (i,s) = f2 seed
                      in (f1 i,s)


generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f g1 g2 = genTwo g1 (\a -> genTwo g2 (\b -> mkGen ((f a b))))

repRandom :: [Gen a] -> Gen [a]
repRandom []  = mkGen ([])
repRandom (x:xs) = genTwo x (\a-> genTwo (repRandom xs) (\b-> mkGen (a:b)))

----------------------------------------------------------------------------------------------
class Monad m where
   return :: a -> m a
   bind :: m a -> (a -> m b) -> m b
 
---Single unified impl for generalB and yLink
---------------------------------------------------------------------

instance  Monad [] where
	return x = [x]
	bind list f = concat (map f list) 

data Maybe a = Just a | Nothing
instance  Monad (Maybe) where
	return a = Just a
	bind Nothing f = Nothing
	bind (Just a) f = f a

evalGen :: Gen1 a -> Seed -> a
evalGen (SM state) = \seed -> (fst(state seed))

data Gen1 a= SM(Seed->(a,Seed))
instance  Monad (Gen1) where
	return a = SM(\seed -> (a,seed))
	bind (SM sx) f = SM sx'
	  where sx' = \seed -> let (i1,s1) = sx seed
	                           SM sx'' = f i1
              			   in sx'' s1


--------------------------------------------------------------------------------------              			  

sequence [] = return []
sequence (x:xs) = bind x (\a-> bind (sequence xs) (\b -> return (a:b)))


liftM2 :: Monad m => (a->b->c) -> m a -> m b -> m c
liftM2 f f1 f2 = bind f1 (\a-> bind f2 (\b-> return (f a b)))

ap _ [] = []
ap [] _ = []
ap (f:fs) xs = (map f xs) ++ ap fs xs 