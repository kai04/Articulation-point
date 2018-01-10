import Control.Applicative
import Control.Monad(liftM,ap)
data Exp = V Var | PP Var | Add Exp Exp
data Var = A|B|C 
	deriving  (Eq,Show)

type State = Var -> Int

data Statemonad s a = SM (s -> (a,s))

instance Functor (Statemonad s) where
	fmap = liftM

instance Applicative (Statemonad s) where
	pure = return
	(<*>)=ap

instance  Monad (Statemonad s) where
	return i = SM(\st -> (i,st))
	(SM sx) >>=k = SM sx'
		where sx' = \st -> let (i1,s1)= sx st
		                       (SM sx'')= k i1
						   in  (sx'' s1)