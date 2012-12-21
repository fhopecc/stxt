import Database.HDBC.Sqlite3
import Database.HDBC
import System.IO

main = do 
    f <- openFile "/stxt/web_mis/sql/create_tables.sql" ReadMode
    sql <- hGetContents f
    print sql
    db <- connectSqlite3 "/stxt/web_mis/db/hltb.db"
    runRaw db sql
    --disconnect db
    hClose f

