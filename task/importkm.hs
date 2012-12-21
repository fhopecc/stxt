import Database.HDBC
import Database.HDBC.Sqlite3
--import Data.ByteString.UTF8
--import System.IO.UTF8 as U
import System.IO
import Text.ParserCombinators.Parsec
import Data.List

main = do
    importData

exportData = do 
    db <- connectSqlite3 "km_trans.db"

importData = do
    db <- connectSqlite3 "km_trans.db"
    f <- openFile "create_kms.sql" ReadMode
    sql <- hGetContents f
    runRaw db sql
    f2 <- openFile "insert_kms_data.sql" ReadMode
    hSetEncoding f2 utf8
    datasql <- hGetContents f2
    runRaw db datasql
    commit db
    disconnect db

readDataCSV = do
    f <- openFile "KM_Cate.csv" ReadMode
    hSetEncoding f utf8
    c <- hGetContents f 
    case parseCSV c of
        Left e -> print e
        Right r -> do
            f2 <- openFile "insert_kms_data.sql" WriteMode
            hSetEncoding f2 utf8
            hPutStr f2 (unlines (map makeValueRow r))
            hFlush f2
    where
        makeValueRow::[String] -> String
        makeValueRow [idno,cate_name,ref] =
            "insert into km_cate(idno, cate_name, ref) values " ++ 
            "(" ++ idno ++ ",\"" ++ cate_name ++ "\"," ++ ref ++ ");"
        makeValueRow x = fail "something"


{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
csvFile :: GenParser Char st [[String]]
csvFile = 
    do result <- many line
       eof
       return result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = 
    do result <- cells
       eol                       -- end of line
       return result
       
-- Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
cells :: GenParser Char st [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)            -- Found comma?  More cells coming
    <|> (return [])                -- No comma?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cellContent :: GenParser Char st String
cellContent = 
    many (noneOf ",\n")
       

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

