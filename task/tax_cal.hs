import IO
--income_tax :: Double -> Double
income_tax p | p >  120000 = min (p*0.17) ((p-120000)*0.5)
             | p <= 120000 = 0

main = do
         putStr "Please enter profit:"
         hFlush stdout 
         p <- getLine 
         print (income_tax (read p::Double)) 
