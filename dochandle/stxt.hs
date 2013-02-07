import Text.ParserCombinators.Parsec

main = do
    run title "cdejjkdj"

test :: Parser Char
test  = char 'c'

title :: Parser [String]
title = do
    lines <- many1 line
    many1 $ char '-'
    return lines

line :: Parser String
line = do
    l <- many1 $ noneOf "\n"
    return l

run p input
    = case (parse p "" input) of
        Left err -> do putStr "error at "
                       print err
        Right x  -> print x
