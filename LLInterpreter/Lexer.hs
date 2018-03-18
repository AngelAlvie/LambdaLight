module Lexer (
    tokenize
) where 

type Token = String

tokenize :: String -> [Token]
tokenize "" = []
tokenize (x:xs) = 