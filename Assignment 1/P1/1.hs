main = print(smallest f (-5) 5)

smallest f a b | a==b = a
               | f a < f (smallest f (a+1) b) = a
               |otherwise = smallest f (a+1) b
f x = x^2-4*x+2 

--coeffs 0 b = (0,1)
  --     a b | coeffs b (a mod b)
