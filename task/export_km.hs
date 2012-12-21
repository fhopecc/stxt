import Database.HDBC
--import Database.HDBC.Sqlite3
--import Data.ByteString.UTF8
--import System.IO.UTF8 as U
import System.IO

main = do 
    db <- connectSqlite3 "km_trans.db"
    r <- quickQuery' db "SELECT idno, cate_name, ref from km_cate" []
    let lines = map convRow r
    f <- openFile "testkm.csv" WriteMode
    hSetEncoding f utf8
    mapM_ (hPutStr f) lines
    hClose f
    where 
        convRow:: [SqlValue] -> String
        convRow[idno, cate_name, ref] = 
            sidno ++ "," ++ scate_name ++ "," ++ sref ++ "\n"
            where
                sidno      = (fromSql idno)::String
                scate_name = (fromSql cate_name)::String
                sref       = (fromSql ref)::String
        convRow x = "failed at " ++ show x
