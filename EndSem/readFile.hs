import System.IO
import System.Environment

main = do
		   args <- getArgs
		   input <- readFile (head args)
		   let textIn = lines input
		   write (textIn)


write [] = putStrLn ""
write (x:xs) = do 
	             appendFile "temp" (x++"\n\n")
	             write xs
            

