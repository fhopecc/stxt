import Mobian
import Parser
import Data.Maybe
import Data.List
import System.IO
import System.Exit
import CP950

main = do 
    db <- loadDB "d:/stxt/mo-bian/family2.mb"   
    hSetEncoding stdin  cp950
    hSetEncoding stdout cp950
    loop db
    where
        loop db = do
            putStr "?-"    
            hFlush stdout
            input <- getLine
            print $ case parseGoal input of
                         Left  e  -> []
                         Right g -> db
--(computeAnswer . snd) $ refute g db
            if input == ":exit" 
            then exitSuccess 
            else loop db
