import Control.Applicative
import Control.Monad(liftM,ap)
import Control.Monad.State.Lazy


data Tree a b = Node a (Tree a b) (Tree a b) | Leaf b
	deriving Show

tree = Node "a" (Node "b" (Leaf 4) (Leaf 6) ) (Leaf 7)

instance Functor (Tree a) where
	fmap f (Node a l r) =  Node a (fmap f l) (fmap f r)
	fmap f (Leaf b) = Leaf (f b)



tree1 = Node 1 (Leaf (*1)) (Node 2 (Leaf (*2)) (Leaf (*3)))
tree2 = Node 10 (Leaf 4)  (Node 20 (Leaf 5) (Leaf 6) )
instance Applicative (Tree a) where
	pure a = Leaf a
	(Node a l r) <*> t  = Node a (l <*> t) (r <*> t)  
	--Leaf f <*> (Node a l r) = Node a (Leaf f <*> l) (Leaf f <*> r)
	--Leaf f <*> Leaf a = Leaf (f a)
	Leaf f <*> t = fmap f t

instance Monad (Tree a) where
		return = pure
		(Leaf a) >>= k = k a
		(Node a l r) >>= k = Node a (l >>= k) (r >>= k)

k = do
	      x<-Node 10 (Leaf 4) (Node 20 (Leaf 5) (Leaf 6))
	      Node 1 (Leaf (x*1)) (Node 2 (Leaf (x*2)) (Leaf (x*3)))

----------------------------------------------------------------------------------------------------------------------
data Expr = I Var | PP Var | Add Expr Expr | Sub Expr Expr
          | If BExpr Expr Expr | Label String Expr
data BExpr = Gt Expr Expr | Not BExpr
data Var = X | Y | Z
  deriving (Eq,Show)


--data StateMonad s a = SM(s -> (a,s)) 

--instance  Functor (StateMonad s) where
--	fmap = liftM

--instance  Applicative (StateMonad s) where
--	pure = return
--	(<*>) = ap

--instance Monad (StateMonad s) where
--	return a = SM(\s->(a,s)) 
--	(SM sx)>>=k = SM sx'
--	  where sx' = \st -> let (a,s) = sx st
--	                         SM sx'' = k a
--	                     in sx'' s


type MyState = (Var->Int,[(String,Int)])
--get :: StateMonad State State
--get = SM(\s-> (s,s) )  
--put :: State -> StateMonad State ()
--put s = SM (\s'->((),s) )


eval :: Expr -> State MyState Int
eval (I v) = do 
	           (f,_)<-get
	           return (f v)
eval (PP v) = do
                (f,l)<-get 
                let oldval= f v
                put (update v (oldval+1) f,l)
                return oldval

eval (Add e1 e2) = do
                     v1<-eval e1
                     v2<-eval e2
                     return (v1+v2)                


eval (Sub e1 e2) = do
                     v1<-eval e1
                     v2<-eval e2
                     return (v1-v2)                     

eval (If c et ef) = do 
                       c<-eval1 c                     
                       if c == True then (eval et) else (eval ef)

eval (Label s e) = do
                     (f,l)<-get
                     put (f,(s,1):l)
                     eval e

eval1 :: BExpr -> State MyState Bool
eval1 (Gt e1 e2) = do
                     v1<-eval e1
                     v2<-eval e2
                     if (v1>v2) then (return True) else (return False)

eval1 (Not e) = do
                     v<-eval1 e
                     if (v==False) then (return True) else (return False)

update v newval f = (\x-> if x==v then newval else (f x))


initstate :: MyState
initstate = ((\x->0),[])                                            

--execute (SM sx) = sx
sampleExpr = If (Gt (I X) (I Y))
                (Label "L1" (I X))
                (Sub (Label "L2" (PP X)) (Label "L3" (Add (I X) (I Y))))
myprint (a,(f,l)) = (a,(f X,f Y,f Z),l)

--k2=
--k1=myprint ( (execute (eval sampleExpr) initstate )) 