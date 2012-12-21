import Database.HDBC.Sqlite3
import Database.HDBC
import System.IO

main = do 
    db <- connectSqlite3 "plant.db"
    run db "INSERT INTO plant(name,ename) VALUES (?, ?)" 
            [toSql "zero", toSql "abc"]
    disconnect db

