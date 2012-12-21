import GHC.IO.Encoding.CP950
import System.IO
main = do
    h <- openFile "hinet.txt" ReadMode
    hSetEncoding h cp950
    s <- hGetContents h
    hSetEncoding stdout cp950
    putStr "張簡稜剛\n問了一句話。\n"
    putStr s

