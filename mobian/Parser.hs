module Parser(parseDB, parseGoal, loadDB) where
import System.IO
import System.Exit
import GHC.IO.Encoding.CP950
import Mobian
import Text.ParserCombinators.Parsec
import Data.Either
import Control.Monad

loadDB :: String -> IO [Rule]
loadDB path = do
    h <- openFile path ReadMode 
    hSetEncoding h utf8
    input <- hGetContents h
    hSetEncoding stdout cp950
    forM_ input print
    forM_ "張簡稜剛." print
        
    let f = case parseDB input of 
                Left e -> [Fact $ Atom "err"]
                Right db -> db
    print f
    return $ f


parseDB :: String -> Either ParseError [Rule]
parseDB input = parse program "MoBianDB" input

parseGoal input = parse goal "MoBianGOAL" input

goal :: GenParser Char st Goal
goal = do 
    ts <- terms
    char '.'
    return $ Goal ts

program :: GenParser Char st [Rule]
program = do 
    first <- try rule <|> clause
    more  <- remainingProgram
    return (first:more)

remainingProgram :: GenParser Char st [Rule]
remainingProgram = do
    (char '\n' >> program)
    <|> (return [])

identifer :: GenParser Char st String
identifer = do 
    str <- many (noneOf " $(),.:-\n")   
    return str

-- ex 張簡稜剛.
clause :: GenParser Char st Rule
clause = do 
    t <- term
    char '.' 
    return $ Fact t

-- ex 祖父($祖父,$子) :- 父親($祖父,父親($父,$子))
rule :: GenParser Char st Rule
rule = do 
    head <- term 
    string " :- "
    tail <- terms
    char '.' 
    return $ (head :- tail)

term :: GenParser Char st Term
term = do
    t <- var <|> try func <|> try atom
    return t

-- ex 父親($父,$子)
var :: GenParser Char st Term
var = do
    char '$'
    name <- identifer
    return $ Var name

-- ex 張簡稜剛
atom :: GenParser Char st Term
atom = do
    name <- identifer
    return $ Atom name

-- ex 父親(張簡金水,張簡稜剛)
func :: GenParser Char st Term
func = do 
    name <- identifer
    char '('
    args <- terms
    char ')'
    return $ Func name args

terms :: GenParser Char st [Term]
terms = do
    first <- var <|> try func <|> atom
    next <- remainingTerms
    return (first:next)

remainingTerms :: GenParser Char st [Term]
remainingTerms = do
    (char ',' >> terms)
    <|> (return [])  
