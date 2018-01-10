--convert number = do 
--	       let first = number `mod` 10
--	       let answershowFirst first 


--showFirst n | n == 1 =
getInt :: IO Int
getInt = readLn 
main = do number <- getInt
          print (convert number 1)

convert number i | number == 0 = ""
				 | i==1 = exceptional (number `mod` 100) number
--				 | i==1 = first (number `mod` 10) number
				 | i==2 = second (number `mod` 10) number
				 | i==3 = third (number `mod` 10) number
				 | i==4 = fourth (number `mod` 10) number
				 | i==5 = fifth (number `mod` 10) number
				 | i==6 = sixth (number `mod` 10) number
				 

exceptional n number 
        | n==11 =  ((convert (number `quot` 100) 3)++"and eleven")
        | n==12 =  ((convert (number `quot` 100) 3)++"and twelve")
        | n==13 =  ((convert (number `quot` 100) 3)++"and thirteen")
        | n==14 =  ((convert (number `quot` 100) 3)++"and fourteen")                    
        | n==15 =  ((convert (number `quot` 100) 3)++"and fifteen")
        | n==16 =  ((convert (number `quot` 100) 3)++"and sixteen")
        | n==17 =  ((convert (number `quot` 100) 3)++"and seventeen")
        | n==18 =  ((convert (number `quot` 100) 3)++"and eighteen")
        | n==19 =  ((convert (number `quot` 100) 3)++"and nineteen")
		| otherwise =  first (number `mod` 10) number

       
first n number 
        | n==1 =  ((convert (number `quot` 10) 2)++"one")
        | n==2 =  ((convert (number `quot` 10) 2)++"two")
        | n==3 =  ((convert (number `quot` 10) 2)++"three")
        | n==4 =  ((convert (number `quot` 10) 2)++"four")                    
        | n==5 =  ((convert (number `quot` 10) 2)++"five")
        | n==6 =  ((convert (number `quot` 10) 2)++"six")
        | n==7 =  ((convert (number `quot` 10) 2)++"seven")
        | n==8 =  ((convert (number `quot` 10) 2)++"eight")
        | n==9 =  ((convert (number `quot` 10) 2)++"nine")
        | n==0 =  ((convert (number `quot` 10) 2)++"")


second n number
         | n==1 =  ((convert (number `quot` 10) 3)++"one ")
         | n==2 =  ((convert (number `quot` 10) 3)++"and twenty ")
         | n==3 =  ((convert (number `quot` 10) 3)++"and thirty ")
         | n==4 =  ((convert (number `quot` 10) 3)++"and fourty ")                    
         | n==5 =  ((convert (number `quot` 10) 3)++"and fifty ")
         | n==6 =  ((convert (number `quot` 10) 3)++"and sixty ")
         | n==7 =  ((convert (number `quot` 10) 3)++"and seventy ")
         | n==8 =  ((convert (number `quot` 10) 3)++"and eighty ")
         | n==9 =  ((convert (number `quot` 10) 3)++"and ninety ")
         | n==0 =  ((convert (number `quot` 10) 3)++"")


third n number
         | n==1 =  ((convert (number `quot` 10) 4)++"one hundred ")
         | n==2 =  ((convert (number `quot` 10) 4)++"two hundred ")
         | n==3 =  ((convert (number `quot` 10) 4)++"three hundred ")
         | n==4 =  ((convert (number `quot` 10) 4)++"four hundred ")                    
         | n==5 =  ((convert (number `quot` 10) 4)++"five hundred ")
         | n==6 =  ((convert (number `quot` 10) 4)++"six hundred ")
         | n==7 =  ((convert (number `quot` 10) 4)++"seven hundred ")
         | n==8 =  ((convert (number `quot` 10) 4)++"eight hundred ")
         | n==9 =  ((convert (number `quot` 10) 4)++"ninety hundred ")
         | n==0 =  ((convert (number `quot` 10) 4)++"")

fourth n number
        | n==1 =  ((convert (number `quot` 10) 5)++"one thousand and ")
        | n==2 =  ((convert (number `quot` 10) 5)++"two thousand and ")
        | n==3 =  ((convert (number `quot` 10) 5)++"three thousand and ")
        | n==4 =  ((convert (number `quot` 10) 5)++"four thousand and ")                    
        | n==5 =  ((convert (number `quot` 10) 5)++"five thousand and ")
        | n==6 =  ((convert (number `quot` 10) 5)++"six thousand and ")
        | n==7 =  ((convert (number `quot` 10) 5)++"seven thousand and ")
        | n==8 =  ((convert (number `quot` 10) 5)++"eight thousand and ")
        | n==9 =  ((convert (number `quot` 10) 5)++"nine thousand and ")
        | n==0 =  ((convert (number `quot` 10) 5)++"thousand and ")


fifth n number
         | n==1 =  ((convert (number `quot` 10) 6)++"one ")
         | n==2 =  ((convert (number `quot` 10) 6)++"twenty ")
         | n==3 =  ((convert (number `quot` 10) 6)++"thirty ")
         | n==4 =  ((convert (number `quot` 10) 6)++"fourty ")                    
         | n==5 =  ((convert (number `quot` 10) 6)++"fifty ")
         | n==6 =  ((convert (number `quot` 10) 6)++"sixty ")
         | n==7 =  ((convert (number `quot` 10) 6)++"seventy ")
         | n==8 =  ((convert (number `quot` 10) 6)++"eighty ")
         | n==9 =  ((convert (number `quot` 10) 6)++"nintey ")
         | n==0 =  ((convert (number `quot` 10) 6)++"")

sixth n number
         | n==1 =  ((convert (number `quot` 10) 7)++"one hundred and ")
         | n==2 =  ((convert (number `quot` 10) 7)++"two hundred and ")
         | n==3 =  ((convert (number `quot` 10) 7)++"three hundred and ")
         | n==4 =  ((convert (number `quot` 10) 7)++"four hundred and ")                    
         | n==5 =  ((convert (number `quot` 10) 7)++"five hundred and ")
         | n==6 =  ((convert (number `quot` 10) 7)++"six hundred and ")
         | n==7 =  ((convert (number `quot` 10) 7)++"seven hundred and ")
         | n==8 =  ((convert (number `quot` 10) 7)++"eight hundred and ")
         | n==9 =  ((convert (number `quot` 10) 7)++"nine hundred and ")
         | n==0 =  ((convert (number `quot` 10) 7)++"")
