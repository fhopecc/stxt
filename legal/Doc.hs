import Text.ParserCombinators.Parsec

main = do
    case parseDoc "title\nbadddfd\n----------\n\n" of
        Left  e -> print e
        Right d -> print d

docFile :: GenParser Char st Doc
docFile = do 
    result <- title
    eof
    return $ EmptyDoc result

title :: GenParser Char st Title
title = do
    title <- tline `sepEndBy1` newline 
    skipMany1 $ char '-'
    newline
    newline
    return title

tline :: GenParser Char st String
tline = do
    str <- many (noneOf "-") 
    return str

parseDoc :: String -> Either ParseError Doc
parseDoc input = parse docFile "(Doc)" input

type Numbers = [Int]

type Title   = [String]

type Items   = [Item]

type Elems   = [Elem]

data Doc     = Doc Title Items
             | EmptyDoc Title
             deriving (Show)

data Item    = Item Numbers Elems
             deriving (Show)

data Elem    = Para  [String] 
             deriving (Show)

