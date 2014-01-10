module STXT( run
           , line, para
           , code
           , content, contents
           , sect2title
           , sect1, sect1s
           , sect2, sect2s
           ) 
where

import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import Control.Monad

data Sect1    = Sect1 Title Sect2s
                deriving (Show)

data Sect2    = Sect2 Title Contents
                deriving (Show)

data Content  = Para Lines
              | Code String
                deriving (Show)

type Title    = String
type Lines    = [String]
type Contents = [Content]
type Sect2s   = [Sect2]

--sect1s = sect1 `sepEndBy` (char '\n') 
sect1s = many sect1

sect1 :: Parser Sect1
sect1 = do
    t  <- sect1title
    cs <- sect2s 
    return $ Sect1 t cs 

sect1title :: Parser Title
sect1title = do
    title <- many $ noneOf "\n"; char '\n'
    skipMany1 $ char '='; string "\n\n"
    return title

--sect2s = sect2 `sepEndBy` (char '\n') 
sect2s = many sect2

sect2title :: Parser Title
sect2title = do
    title <- many1 $ noneOf "\n"; char '\n'
    skipMany1 $ char '-'; string "\n\n"
    return title

sect2 :: Parser Sect2
sect2 = do notFollowedBy sect1title
           t  <- sect2title
           cs <- contents 
           return $ Sect2 t cs 

contents = many content

content =  code
       <|> (do p <- para
               many $ char '\n'
               return p
           )

code :: Parser Content
code = do string "碼：\n\n"
          src <- manyTill anyChar (try (string "\n\n\n"))
          return $ Code src

para :: Parser Content
para  = do ls <- line `sepEndBy1` (char '\n')
           return $ Para ls

line :: Parser String
line = do notFollowedBy sect1title
          notFollowedBy sect2title  
          head <- noneOf "\n"
          tail <- try (manyTill (noneOf "\n") 
                                (lookAhead (string "碼：\n\n")))
              <|> (many $ noneOf "\n")
          return $ head:tail

run p input
    = case (parse p "" input) of
        Left err -> show err
        Right x  -> show x

main = do
    args <- getArgs 
    let src  = args !! 0
    f <- openFile src ReadMode
    hSetEncoding f utf8
    c <- hGetContents f
    let o = run sect1s c
    putStr $ o
