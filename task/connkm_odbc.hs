import Database.HDBC
import Database.HDBC.ODBC
import System.IO
import Data.Char
import qualified Data.ByteString as B
import Data.ByteString.CP950
import GHC.IO.Encoding.CP950

main = do
    catchSql accessDB $ \e -> do
        hSetEncoding stdout cp950
        let mst = seErrorMsg e
        putStrLn $ readLit mst ""
    where 
        readLit ""  out = reverse out
        readLit src out = 
            let [(c, rest)] = readLitChar src
            in readLit rest (c:out)

accessDB = do 
    db <- connectODBC "Driver={Sql Server};Server=192.168.1.8;uid=eltweb;pwd=bewt_111"
    r <- quickQuery' db "SELECT recev_no, tax_cd, remark from yrxt002" []
    let lines = map convRow r
    f <- openFile "testkm.csv" WriteMode
    hSetEncoding f utf8
    mapM_ (hPutStr f) lines
    hClose f
    where 
        convRow:: [SqlValue] -> String
        convRow[recev_no, tax_cd, remark] = 
            srecev_no ++ "," ++ stax_cd ++ "," ++ sremark ++ "\n"
            where
                srecev_no  = (fromSql recev_no)::String
                stax_cd = (fromSql tax_cd)::String
                sremark       = decodeCP950((fromSql remark)::B.ByteString)
        convRow x = "failed at " ++ show x
