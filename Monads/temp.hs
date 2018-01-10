data Exp = V Var | Add Exp Exp | Div Exp Exp | PP Var
data Var = A | B | C

initiate :: Var -> Int
initiate A = 1
initiate B = 2
initiate C = 3

type State = Var -> Int
type StateMonad a = State -> (a,State)

return :: a -> StateMonad a
return i = \state -> (i,state) 

eval :: Exp -> StateMonad Int
eval (V v) = \state -> (state v, state) 

eval (Add e1 e2) = do
						i1 <- eval e1
						i2 <- eval e2
						Main.return (i1+i2)

myprint (v,s) = (v,[s A,s B,s C])