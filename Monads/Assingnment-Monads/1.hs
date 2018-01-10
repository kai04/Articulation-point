import MCPrelude

type Gen x = Seed -> (x,Seed)  -------type Synonyms


fiveRands :: [Integer]
fiveRands = fiveRands' 5 (mkSeed 1) 

fiveRands'  0 _ = []
fiveRands' k seed = let(i,s) = rand seed
					in i:fiveRands' (k-1) s

mult [] = 1
mult (x:xs) = x * mult xs

---------------------------------------------------------------------------

randLetter :: Gen Char
randLetter seed = let (i,s) = rand seed
				  in (toLetter i,s)

randString3 :: String
randString3 = randString3' 3 (mkSeed 1)

randString3' 0 _ = []
randString3' k seed = let (c,s)=randLetter seed
					  in c:randString3' (k-1) s

----------------------------------------------------------------------------

generalA :: (a->b) -> Gen a -> Gen b
generalA f1 f2 seed = let (i,s) = f2 seed
                      in (f1 i,s)

randEven :: Gen Integer
randEven seed = generalA (*2) (rand) seed

randOdd :: Gen Integer
randOdd seed = generalA (+1) (randEven) seed  

randTen :: Gen Integer
randTen seed = generalA (*10) (rand) seed

--------------------------------------------------------------

randPair :: Gen (Char,Integer)
randPair seed = let (c,s1) = randLetter seed
                    (i,s2) = rand s1
                in ((c,i),s2)


generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair f1 f2 seed = let (i1,s1) = f1 seed
                             (i2,s2) = f2 s1
						 in  ((i1,i2),s2)


generalB :: (a->b->c) -> Gen a -> Gen b -> Gen c
generalB f f1 f2 seed = let (i1,s1) = f1 seed
                            (i2,s2) = f2 s1
                        in  (f i1 i2,s2) 

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 f1 f2 seed = generalB (,) f1 f2 seed


-------------------------------------------------------------------------

repRandom :: [Gen a] -> Gen [a]
repRandom [] seed = ([],seed)
repRandom (x:xs) seed = let (i,s) = x seed
                            (r,s2) = repRandom xs s  
						in  (i:r,s2)

--------------------------------------------------------------------------

genTwo ::Gen a -> (a->Gen b)-> Gen b
genTwo f1 f2 = \seed -> let (i,s)=(f1 seed)
                        in f2 i s 

mkGen :: a -> Gen a
mkGen x seed = (x,seed)