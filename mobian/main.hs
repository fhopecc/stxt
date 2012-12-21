module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr(args !! 0))



readExpr :: String -> String
readExpr input = case parse parseAtom "mo-bian" input of
                 Left err -> " No match : " ++ show err
                 Right val -> (strMoBianData val)

data MoBianData = Atom String     -- atom(string)
                | Number Integer 
                | Variable String -- variable(name)

strMoBianData (Atom x) = x

parseAtom :: Parser MoBianData
parseAtom = do first <- letter
               rest <- many (letter <|> digit <|> symbol)
               let string = first:rest
               return $ Atom string

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"
