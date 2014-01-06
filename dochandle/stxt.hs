module STXT(run
           ,line, para, paras
           ,sect1title
           ) 
where

import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import Control.Monad

data Sect1    = Sect1 Title Paras

type Title    = String
type Para     = Lines
type Lines    = [String]
type Paras    = [Para]
type Integers = [Integer]

line :: Parser String
line = do head <- noneOf "=-\n"
          tail <- many $ noneOf "\n"
          return $ head:tail

para  = line `sepEndBy1` (char '\n')

paras = para `sepEndBy` (many $ char '\n')

sect1title :: Parser Title
sect1title = do
    title <- line; char '\n'
    skipMany1 $ char '='; char '\n'
    return title

run p input
    = case (parse p "" input) of
        Left err -> show err
        Right x  -> show x

--main = do
--    args <- getArgs 
--    let src  = args !! 0
--    f <- openFile src ReadMode
--    c <- hGetContents f
--    let o = run document c
--    putStr $ o
