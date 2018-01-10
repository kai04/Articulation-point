import Control.Applicative
import Control.Monad(liftM,ap)
import System.Random

----------------------------------1------------------------------------------------------
data LogSqrt = Val Float | Log LogSqrt | Sqrt LogSqrt

eval :: LogSqrt -> Maybe Float
eval (Val v) = return v
eval (Log e) = eval e >>= \x-> if x<0 then Nothing else return (log x)  
--------------------------------------------------------------------------

------------------------------2----------------------------------------
data StateMonad s a = SM( s-> (a,s))
instance  Functor (StateMonad s) where
	fmap = liftM

instance  Applicative (StateMonad s) where
	pure = return
	(<*>) = ap

instance Monad (StateMonad s) where   			-- *->* is the kind of the type constructor Maybe  --  Cant just write StateMonad bcoz it takes two types so have to write like this
	return i = SM(\s -> (i,s))
	(SM sx) >>= k = SM sx'
		where sx' = \st -> let (i,st1) = sx st
		                       (SM sx'') = k i
						   in sx'' st1


identifiers_helper :: StateMonad String [String]       ----------It will take string and returns list of strings  --- SM(String -> ([String],String))
matches :: StateMonad String String 					-------------SM(String -> (String,String))  					

matches = SM(head.lex)

identifiers_helper = do
					  	v <- matches 
					  	if (v == []) then return [] 
					  	else
					  		do
					  			vs <- identifiers_helper
					  			return (v:vs)


identifiers str = fst (sx str)
	where (SM sx) = identifiers_helper

------------------------------------------------------------------------------------------------------------------------

generateRandoms :: Int -> Int -> [Int]
generateRandoms k seed = generateRandoms_helper k (mkStdGen seed) 

generateRandoms_helper 0 stdgen = []
generateRandoms_helper k stdgen = let (i1,stdgen') = (next stdgen)  
								  in i1:generateRandoms_helper (k-1) stdgen'

---------------------------------------------------------------------------------------------------------------------------

data Exp = V Var | Add Exp Exp | Sub Exp Exp | IF BExp Exp Exp | PP Var
data BExp = Gt Exp Exp | Not BExp
data Var = X|Y|Z
	deriving (Eq,Show)

type State = (Var->Int,Int,Int)

eval1 :: Exp -> StateMonad State Int
eval1 (V v) = SM(\(f,c,d) -> (f v,(f,c,d)))
eval1 (Add e1 e2) = do 
						i1 <- eval1 e1
						i2 <- eval1 e2
						SM(\(f,c,d)-> ((i1+i2),(f,c+1,d))) 

eval1 (Sub e1 e2) = do 
						i1 <- eval1 e1
						i2 <- eval1 e2
						SM(\(f,c,d)-> ((i1+i2),(f,c,d+1))) 

eval1 (PP v) = SM(\(f,c,d)-> (f v,(update f v ((f v)+1),c+1,d)))
	where update f var val = \var' -> if (var == var') then val else f var'  


eval2 :: BExp -> StateMonad State Bool 
eval2 (Gt e1 e2) = do 
						i1 <- eval1 e1
						i2 <- eval1 e2
						if (i1>i2) then return True else return False 

eval2 (Not e1) = do 
						i1 <- eval2 e1
						if (i1==False) then return True else return False 


initstate :: (Var->Int,Int,Int)
initstate = (f,0,0)
    where f X =0
          f Y =1
          f Z =2 

myprint (v,(f,c,d)) = (v,[f X,f Y,f Z,c,d])
--let (SM sx) = eval1 (Add (V X) (V Y))
--in myprint (sx initstate)