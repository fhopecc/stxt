module STXT(Document(Document, Error, Ints)
           ,Title(Title, NTitle)
           ,stxt
           ,run
           ,line, para, paras
           ,title
           ,num, nums, ntitle
           ,section, sections
           ,document
           ) 
where

import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import Control.Monad

data Document = Document Title Sections
              | Error String
              | Ints Integers
              deriving (Eq, Show)

data Title    = Title  Lines
              | NTitle Integers String
              deriving (Eq, Show)

data Section  = Section Title Paras
              deriving (Eq, Show)

type Para     = Lines
type Sections = [Section]
type Lines    = [String]
type Paras    = [Para]
type Integers = [Integer]

line = do head <- noneOf "0123456789\n"
          tail <- many $ noneOf "\n"
          return $ head:tail

para  = line `sepEndBy1` (char '\n')

paras = para `sepEndBy` (many $ char '\n')

num  :: Parser Integer
num = do
    n <- many1 digit
    return $ read n

nums = num `endBy1` (char '.')

ntitle :: Parser Title
ntitle = do 
    ns     <- nums
    title  <- line <|> (return "")
    return $ NTitle ns title

section = do{
            title <- ntitle;
            do {
                many $ char '\n';
                ps <- paras;
                return $ Section title ps;
               }
            <|> (return $ Section title [])
           }

--sections = section `sepEndBy`  (char '\n')
sections = many section

title :: Parser Title
title = do
    lines <- (many1 $ noneOf "-\n") `endBy` (string "\n")
    many1 $ char '-'
    return $ Title lines

document = do
    t <- title
    many $ char '\n'
    content <- sections
    return $ Document t content

doc :: Parser Document
doc = do 
    t <- title
    return $ Document t []

run p input
    = case (parse p "" input) of
        Left err -> show err
        Right x  -> show x

stxt :: String -> Document
stxt input = case (parse doc "" input) of
        Left err -> Error $ show err
        Right x  -> x

main = do
    args <- getArgs 
    let src  = args !! 0
    f <- openFile src ReadMode
    c <- hGetContents f
    let o = run document c
    putStr $ o
