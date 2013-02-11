import System
import IO

fac 0 = 1
fac 1 = 1
fac x = x * fac (x - 1)
main = putStr "Enter a number for factory:" >>
       hFlush stdout >>
       getLine >>=
       \ x -> if x == "quit"
                 then exitWith ExitSuccess
                 else print (fac (read x::Int)) >>
       main
