module STXT( run, rawRun, runPart, rawRunPart
           , line, para
           , codePara
           , codeParas
           , content, contents
           , sect2title
           , sect1, sect1s
           , sect2, sect2s
           , doc
           , Doc(Doc, Error)
           , Sect1(Sect1)
           , Sect2(Sect2)
           , Sect1s
           , Sect2s
           , Title
           , Content(Code, Para)
           , getSect2s
           , getSect2sFromSect1
           , ParaObj(Str, Link)
           , paraLink, paraObj, paraObjs
           ) 
where

import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Network.URI

data Doc      = Doc Title Contents Sect1s
              | Error String
                deriving (Show)

type Sect1s   = [Sect1]

data Sect1    = Sect1 Int Title Contents Sect2s
                deriving (Show)

type Sect2s   = [Sect2]

data Sect2    = Sect2 (Int, Int) Title Contents 
                deriving (Show)

data Content  = Para ParaObjs
              | Code String
                deriving (Show)

type Contents = [Content]

data ParaObj = Str String
             | Link Title URL 
               deriving (Show)

type ParaObjs = [ParaObj]

type Title    = String

type Lines    = [String]

type URL  = String -- Maybe use Network.URI to parse

getSect2s :: Int -> Doc -> Sect2s
getSect2s n1 doc@(Doc _ _ s1s) = s2s
    where
        s1@(Sect1 _ _ _ s2s) = fromMaybe (s1s !! 0) (find (isNumEq n1) s1s)
        isNumEq num (Sect1 n _ _ _) = n == num 

getSect2sFromSect1 :: Sect1 -> Sect2s
getSect2sFromSect1 (Sect1 _ _ _ s2s) = s2s

doc :: Parser Doc
doc = do
    t   <- docTitle
    cs  <- contents
    s1s <- sect1s 
    return $ Doc t cs s1s

docTitle = title '='

sect1s = many sect1

sect1 :: Parser Sect1
sect1 = do
    t  <- sect1title
    cs  <- contents
    s2s <- sect2s 
    return $ Sect1 0 t cs s2s

sect1title = title '-'

sect2s = many sect2

sect2title = title '.'

sect2 :: Parser Sect2
sect2 = do notFollowedBy sect1title
           t  <- sect2title
           cs <- contents 
           return $ Sect2 (0,0) t cs

contents = many content

content = choice [ codePara
                 , codeParas
                 ,(do p <- para
                      optional $ char '\n'
                      return p
                  )
                 ]

codePara :: Parser Content
codePara = do notFollowedBy codeParas
              string "：\n\n"
              src <- manyTill anyChar (try (string "\n\n"))
              return $ Code src

codeParas :: Parser Content
codeParas = do string "：：\n\n"
               src <- manyTill anyChar (try (string "\n\n\n"))
               return $ Code src

para :: Parser Content
para  = do ls <- line `sepEndBy1` (char '\n')
           let objs = case parse paraObjs "paraObjs" (concat ls) of
                           Left err -> [Str $ concat ls]
                           Right x  -> x
           return $ Para objs

-- [food|http://fhopehltb.appspot.com/food/food.html]
paraLink :: Parser ParaObj
paraLink = between (char '[') (char ']') ( 
    try( do t <- many $ noneOf "|]\n"
            char '|'
            uri <- many $ noneOf "]\n"
            if (isNothing $ parseURI uri) then
                return $ Str $ "[" ++ t ++ "|" ++ uri ++ "]"
            else
                return $ Link t uri
    )<|>(do r <- many $ noneOf "]\n"
            return $ Str $ "[" ++ r ++ "]"
        )
    )

paraObj :: Parser ParaObj
paraObj =  try paraLink
       <|> (do str <- many1 $ noneOf "[" 
               return $ Str str
           )

paraObjs :: Parser ParaObjs
paraObjs = many1 paraObj

line :: Parser String
line = do notFollowedBy sect1title
          notFollowedBy sect2title  
          head <- noneOf "\n"
          tail <- try (do str <- manyTill (noneOf "\n") 
                                          (lookAhead (string "：\n\n"))
                          return $ str ++ "："
                      )
              <|> (many $ noneOf "\n")
          return $ head:tail

title :: Char -> Parser Title
title sep = do
    t <- many1 $ noneOf "\n"; char '\n'
    string $ replicate 2 sep;many $ char sep; string "\n\n"
    return t

rawRunPart p input
    = case (parse p "" input) of
        Left err -> show err
        Right x  -> show x

runPart p input
    = case (parse p "" input) of
        Left err -> show err
        Right x  -> show $ numberDoc x

rawRun input
    = case (parse doc "" input) of
        Left err -> Error $ show err
        Right x  -> x

run input = 
    case (parse doc "" input) of
        Left err -> Error $ show err
        Right x  -> numberDoc x

numberDoc :: Doc -> Doc
numberDoc (Doc t cs s1s) = Doc t cs (numberSect1s s1s)

numberSect1s :: Sect1s -> Sect1s
numberSect1s s1s = 
    [updateNumSect1 n s1 | (n, s1) <- zip [1..] s1s]
     where 
        updateNumSect1 n (Sect1 _ t cs s2s) = Sect1 n t cs (numberSect2s n s2s)

        numberSect2s s1n s2s = 
            [updateNumSect2 s1n s2n s2 | (s2n, s2) <- zip [1..] s2s]

        updateNumSect2 s1n s2n (Sect2 _ t cs) = Sect2 (s1n, s2n) t cs
   
main = do
    args <- getArgs 
    let src  = args !! 0
    f <- openFile src ReadMode
    hSetEncoding f utf8
    c <- hGetContents f
    let o = run c
    putStr $ show o
