module Lexer (
    tokenize
,   head_words
) where 

import Common

-- This takes in a string (corresponding to a whole expression, on a line.)
tokenize :: String -> [Token]
tokenize "" = []
tokenize (x:xs) = case x of 'λ' -> Lambda : tokenize xs
                            '\\'-> Lambda : tokenize xs
                            '(' -> LParen : tokenize xs
                            ')' -> RParen : tokenize xs
                            '.' -> Dot   : tokenize xs
                            ':' -> if head xs == '=' then Def : tokenize (tail xs)
                                                     else Word "h" : tokenize xs
                            ' ' -> tokenize xs
                            '-' -> if head xs == '-' then [] else tokenize_word (x:xs)
                            -- identify words
                            x -> tokenize_word (x:xs)

tokenize_word :: String -> [Token]
tokenize_word (x:xs) = let ans = (head_words (x:xs)) in Word (fst ans) : (tokenize (snd ans))

-- Splits a string into first word, and the rest of the words
head_words :: String -> (String, String)
-- Handle terminal cases
head_words "" = ("","")
head_words (x:xs) = case x of ' ' -> ("", x:xs)
                              '.' -> ("", x:xs)
                              '(' -> ("", x:xs)
                              ')' -> ("", x:xs)
                              ':' -> ("", x:xs)
                              '\\'-> ("", x:xs)
                              'λ' -> ("", x:xs)
                              x   -> let ans = (head_words xs) in (x : fst ans, snd ans)
