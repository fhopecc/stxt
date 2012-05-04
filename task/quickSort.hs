quicksort []     = []
quicksort [x]    = [x]
quicksort (x:xs) = (quicksort more) ++ (x : equal) ++ (quicksort less)
                    where less  = filter (<  x) xs
                          equal = filter (== x) xs
                          more  = filter (>  x) xs

w = [3,2,1,2,3,2,4,5,1]

main = print (quicksort w)
