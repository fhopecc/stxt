import System.Timeout
main = timeout 18000000 $ loop [1..] 
    where
    loop (x:xs) = do
        print x
        loop xs
    loop [] = return ()
