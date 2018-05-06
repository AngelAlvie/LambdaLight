module Lexer (
    tokenize
,   head_words
) where 

import Common

tokenize :: String ->  [Token]
tokenize "" = []
tokenize (x:xs)
  |  x == 'λ' = Lambda : tokenize xs
  |  x == '\\'= Lambda : tokenize xs
  |  x == '(' = LParen : tokenize xs
  |  x == ')' = RParen : tokenize xs
  |  x == '.' = Dot : tokenize xs
  |  x == ':' && xs /= [] && head xs == '=' = Def : tokenize (tail xs)
  |  x == ' ' = tokenize xs
  |  x == '-' && xs /= [] && head xs == '-' = []
  | otherwise = tokenize_word (x:xs)

tokenize_word :: String -> [Token]
tokenize_word (x:xs) = let ans = (head_words (x:xs)) in Word (fst ans) : (tokenize (snd ans))

-- Splits a string into first word, and the rest of the words
head_words :: String -> (String, String)
-- Handle terminal cases
head_words "" = ("","")
head_words (x:xs)
  | x == ' ' = ("", x:xs)
  | x == '.' = ("", x:xs)
  | x == '(' = ("", x:xs)
  | x == ')' = ("", x:xs)
  | x == ':' = ("", x:xs)
  | x == '\\'= ("", x:xs)
  | x == 'λ' = ("", x:xs)
  | otherwise = let ans = (head_words xs) in (x : fst ans, snd ans)
