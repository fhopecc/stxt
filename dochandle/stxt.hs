module STXT( run
           , line, para
           , code
           , content, contents
           , sect2title
           , sect2, sect2s
           ) 
where

import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import Control.Monad

data Sect2    = Sect2 Title Contents
                deriving (Show)

data Content  = Para Lines
              | Code String
                deriving (Show)

type Title    = String
type Lines    = [String]
type Contents = [Content]

sect2s = sect2 `sepEndBy` (char '\n') 

line :: Parser String
line = do head <- noneOf "=-\n"
          tail <- try (manyTill (noneOf "\n") 
                                (lookAhead (string "碼：\n\n")))
              <|> (many $ noneOf "\n")
          return $ head:tail

para :: Parser Content
para  = do ls <- line `sepEndBy1` (char '\n')
           return $ Para ls

code :: Parser Content
code = do string "碼：\n\n"
          src <- manyTill anyChar (try (string "\n\n"))
          return $ Code src

content =  code
       <|> para 
        
contents = many content

sect2title :: Parser Title
sect2title = do
    title <- line; char '\n'
    skipMany1 $ char '-'; char '\n'
    return title




sect2 :: Parser Sect2
sect2 = do
    t  <- sect2title
    cs <- contents 
    return $ Sect2 t cs 

sect1title :: Parser Title
sect1title = do
    title <- line; char '\n'
    skipMany1 $ char '-'; char '\n'
    return title



run p input
    = case (parse p "" input) of
        Left err -> show err
        Right x  -> show x

main = do
    args <- getArgs 
    let src  = args !! 0
    f <- openFile src ReadMode
    c <- hGetContents f
    let o = run sect2s c
    putStr $ o
