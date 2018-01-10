import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Data.Maybe
import qualified Data.Map as Map

type Name = String
data Exp = Lit Integer
			|Var Name
			|Plus Exp Exp
			|Abs Name Exp
			|App Exp Exp
			deriving(Show)

data Value = IntVal Integer
			|FunVal Env Name Exp
			deriving(Show)

type Env = Map.Map Name Value



eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)  --lookup return Maybe value or nothing if 'n' does not exist.fromJust will throw exception if given Nothing
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
						 in IntVal (i1+i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
						 in case val1 of
						 	  FunVal env' n body -> eval0 (Map.insert n val2 env') body

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
run0=eval0 Map.empty exampleExp
--------------------------------------------------------------------------------------------------------------------------------

eval1 :: Env -> Exp -> Identity Value
eval1 env (Lit i) = return (IntVal i)
eval1 env (Var n) = return (fromJust (Map.lookup n env))
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return (IntVal (i1+i2))
eval1 env (Abs n e) = return (FunVal env n e)
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
						 	  FunVal env' n body -> eval1 (Map.insert n val2 env') body
run1=runIdentity (eval1 Map.empty exampleExp)

----------------------------------------------------------------------------------------------------------------------------

eval2 :: Env -> Exp -> ErrorT String Identity Value
eval2 env (Lit i) = return (IntVal i)
eval2 env (Var n) = case Map.lookup n env of
	                   Nothing -> throwError ("unbound variable: "++n) 
	                   Just val -> return val
eval2 env (Plus e1 e2) = do e1' <- eval2 env e1
                            e2' <- eval2 env e2
                            case (e1',e2') of
                            	(IntVal i1,IntVal i2) -> return (IntVal (i1+i2))
                            	_ -> throwError "type error in addition"
eval2 env (Abs n e) = return (FunVal env n e)
eval2 env (App e1 e2) = do val1 <- eval2 env e1
                           val2 <- eval2 env e2
                           case val1 of
						 	  FunVal env' n body -> eval2 (Map.insert n val2 env') body		
						 	  _ -> throwError "type error in application"				 

run2_1=runIdentity (runErrorT (eval2 Map.empty exampleExp)) 						 -- => Right (IntVal 18) 						 	  
run2_2=runIdentity (runErrorT (eval2 Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))) -- => Left "type error in addition"
run2_3=runIdentity (runErrorT (eval2 Map.empty (Var "x")))					    	 -- => Left "unbound variable: x"


--------------------------------------------------------------------------------------------------------------------------------------

eval3 :: Exp ->  ReaderT Env (ErrorT String Identity) Value 
eval3 (Lit i) = return (IntVal i)
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
	                  Nothing -> throwError ("unbound variable: "++n) 
	                  Just val -> return val
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1',e2') of
                            (IntVal i1,IntVal i2) -> return (IntVal (i1+i2))
                            _ -> throwError "type error in addition"
eval3 (Abs n e) = do env <- ask
                     return (FunVal env n e)
eval3 (App e1 e2) = do val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
						 FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
						 _ -> throwError "type error in application"				 


run3=runIdentity (runErrorT (runReaderT (eval3 exampleExp) Map.empty))


-----------------------------------------------------------------------------------------------------------------------------------------------------
---newtype StateT s m a   --- type of StateT
---runStateT :: StateT s m a -> s -> m (a, s)

tick = do st <- get
          put (st+1)
--eval3 :: Exp → ReaderT Env (ErrorT String Identity) Value
eval4 :: Exp -> ReaderT Env (ErrorT String (StateT Integer Identity)) Value
eval4 (Lit i) = do tick
                   return $IntVal i 
eval4 (Var n) = do 
	               tick
	               env <- ask
	               case Map.lookup n env of 
						Nothing ->throwError ("unbound variable: "++n) 
						Just val-> return val
eval4 (Plus e1 e2) = do 
	                    tick
	                    e1' <- eval4 e1
	                    e2' <- eval4 e2
	                    case (e1',e2') of 
							(IntVal i1,IntVal i2) -> return $IntVal (i1 +i2) 
							_->throwError "type error in addition"
eval4 (Abs n e) = do 
	                 tick
	                 env <- ask 
	                 return $FunVal env n e 
eval4 (App e1 e2) = do
                       tick
                       val1 <- eval4 e1
                       val2 <- eval4 e2
                       case val1 of
                       	FunVal env' n body ->local (const (Map.insert n val2 env')) (eval4 body)
                       	_->throwError "type error in application" 

run4=runIdentity (runStateT (runErrorT (runReaderT (eval4 exampleExp) Map.empty)) 0)  -- => (Right (IntVal 18),8)                       	
--------------------------------------------------------------------------------------------------------------------------------------------
--In the evaluation function, we illustrate the use of the writer monad by writing out the name of each variable encountered during evaluation. 
--newtype Writer w a = Writer { runWriter :: (a, w) }  ---The a type parameter represents the type of the value and the w type parameter the type of the attached monoid value.

--eval5 :: Exp -> ReaderT Env (ErrorT String (WriterT [String] (StateT Integer Identity))) Value
eval5 (Lit i) = do tick
                   return $IntVal i 
eval5 (Var n) = do 
	               tick
	               tell [n]
	               env <- ask
	               case Map.lookup n env of 
						Nothing ->throwError ("unbound variable: "++n) 
						Just val-> return val
eval5 (Plus e1 e2) = do 
	                    tick
	                    e1' <- eval5 e1
	                    e2' <- eval5 e2
	                    case (e1',e2') of 
							(IntVal i1,IntVal i2) -> return $IntVal (i1 +i2) 
							_->throwError "type error in addition"
eval5 (Abs n e) = do 
	                 tick
	                 env <- ask 
	                 return $FunVal env n e 
eval5 (App e1 e2) = do
                       tick
                       val1 <- eval5 e1
                       val2 <- eval5 e2
                       case val1 of
                       	FunVal env' n body ->local (const (Map.insert n val2 env')) (eval5 body)
                       	_->throwError "type error in application" 

