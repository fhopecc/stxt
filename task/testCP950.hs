import GHC.IO.Encoding.CP950
import Data.ByteString.CP950
import System.IO
import qualified Data.ByteString as B

main = test2

test1 = do
    h <- openFile "hinet.txt" ReadMode
    hSetEncoding h cp950
    s <- hGetContents h
    hSetEncoding stdout cp950
    putStr "張簡稜剛\n問了一句話。\n"
    putStr s

test2 = do
    h <- openFile "hinet.txt" ReadMode
    bs <- B.hGetContents h
    B.putStr bs
    let str = decodeCP950 bs
    hSetEncoding stdout cp950
    putStr str
    --putStr "張簡稜剛\n問了一句話。\n"
    --putStr s

