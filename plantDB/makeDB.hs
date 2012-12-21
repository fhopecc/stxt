import Database.HDBC.Sqlite3
import Database.HDBC
import System.IO

main = do 
    f <- openFile "create_tables.sql" ReadMode
    sql <- hGetContents f
    print sql
    db <- connectSqlite3 "plant.db"
    runRaw db sql
    --disconnect db
    hClose f

