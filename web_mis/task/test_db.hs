import Database.HDBC.Sqlite3
import Database.HDBC

main = do
    db <- connectSqlite3 "/stxt/web_mis/db/hltb.db"
    tables <- getTables db
    print tables
    run db 
