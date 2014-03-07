module STXT( run, rawRun, runPart, rawRunPart, runInclude
           , line, para, include
           , codePara
           , codeParas
           , content
           , sect2title
           , sect1, sect1s
           , sect2, sect2s
           , doc
           , Doc(Doc, Error)
           , Sect1(Sect1, Include)
           , Sect2(Sect2)
           , Sect1s
           , Sect2s
           , Title
           , Elem(Code, Para)
           , Content
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
import qualified System.FilePath as FP

data Doc      = Doc Title Content Sect1s
              | Error String
                deriving (Show, Eq)

type Sect1s   = [Sect1]

data Sect1    = Sect1 Int Title Content Sect2s
              | Include FilePath
                deriving (Show, Eq)

type Sect2s   = [Sect2]

data Sect2    = Sect2 (Int, Int) Title Content
                deriving (Show, Eq)

data Elem = Para ParaObjs
          | Code String
            deriving (Show, Eq)

type Content = [Elem]

data ParaObj = Str String
             | Link Title URL 
               deriving (Show, Eq)

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
    t <- docTitle
    c <- content
    s1s <- sect1s 
    return $ Doc t c s1s

docTitle = title '='

sect1s = many (include <|> sect1)

sect1 :: Parser Sect1
sect1 = do
    t  <- sect1title
    cs  <- content
    s2s <- sect2s 
    return $ Sect1 0 t cs s2s

sect1title = title '-'

sect2s = many sect2

sect2title = title '.'

sect2 :: Parser Sect2
sect2 = do notFollowedBy sect1title 
           t  <- sect2title
           cs <- content
           return $ Sect2 (0,0) t cs

content = many theElement

include :: Parser Sect1
include = do filepath <- between (string "<") 
                  (do {char '>'; many $ oneOf "\n"}) 
                  (many (letter <|> oneOf "\\/.:_"))
             return $ Include filepath

theElement = choice [ codePara
              , codeParas
              ,(do p <- para
                   optional $ char '\n'
                   return p
               )
              ]

codePara :: Parser Elem
codePara = do notFollowedBy codeParas
              string "：\n\n"
              src <- manyTill anyChar (try (string "\n\n"))
              return $ Code src

codeParas :: Parser Elem
codeParas = do string "：：\n\n"
               src <- manyTill anyChar (try (string "\n\n\n"))
               return $ Code src

para :: Parser Elem
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
          head <- noneOf "<\n"
          tail <- try (do str <- manyTill (noneOf "\n") 
                                          (lookAhead (string "：\n\n"))
                          return $ str ++ "："
                      )
              <|> (many $ noneOf "\n")
          return $ head:tail

title :: Char -> Parser Title
title sep = do
    t <- many1 $ noneOf "<\n"; char '\n'
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

runSect1 input =
    case (parse sect1 "" input) of
        Left err -> Sect1 0 (show err) [] []
        Right x  -> x

runInclude :: FilePath -> String -> IO Doc
runInclude srcdir input = do
    let d = rawRun input
    case d of  
        (Doc _ _ _) -> do
            d' <- execInclude srcdir d
            return $ numberDoc d'
        (Error msg) -> error msg 

execInclude :: FilePath -> Doc -> IO Doc
execInclude srcdir d@(Doc t cs s1s)= do
    ns1s <- forM s1s $ \s1 -> include2Sect1 s1
    return $ Doc t cs ns1s
    where
        include2Sect1 (Include src) = do
            f <- openFile (FP.combine srcdir src) ReadMode
            hSetEncoding f utf8
            c <- hGetContents f
            return $ runSect1 c

        include2Sect1 s1 = return $ s1 

numberDoc :: Doc -> Doc
numberDoc (Doc t cs s1s) = Doc t cs (numberSect1s s1s)

numberSect1s :: Sect1s -> Sect1s
numberSect1s s1s = 
    [updateNumSect1 n s1 | (n, s1) <- zip [1..] s1s]
     where 
        updateNumSect1 n (Sect1 _ t cs s2s) = Sect1 n t cs (numberSect2s n s2s)
        updateNumSect1 n i@(Include _ ) = i

        numberSect2s s1n s2s = 
            [updateNumSect2 s1n s2n s2 | (s2n, s2) <- zip [1..] s2s]

        updateNumSect2 s1n s2n (Sect2 _ t cs) = Sect2 (s1n, s2n) t cs
   
main = do
    args <- getArgs 
    let src  = args !! 0
    let srcdir = FP.takeDirectory src
    f <- openFile src ReadMode
    hSetEncoding f utf8
    c <- hGetContents f
    let o = run c
    o' <- execInclude srcdir o
    putStr $ show o'
