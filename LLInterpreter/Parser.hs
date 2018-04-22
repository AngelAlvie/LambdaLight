module Parser (
    parse
) where

import Lexer (Token (LParen, RParen, Word, Lambda, Dot, Def, End)) 
import Evaluator (Expr (Var, Abs, App, Bind, Prim))

-- The Parser should take in a list of tokens, and construct an Abstract Syntax Tree based on the Lexs that are returned by the lexer,
-- The AST is defined in terms of the Evaluator Expr type.

-- The parse function is just a dispatcher that checks if the token list implies a particular type of expression,
-- And calls the appropriate sub-parser to handle it
parse :: [Token] -> (Expr, [Token])
parse x
  | is_parens x = parse_parens x
  | is_abs x = parse_abs x
  | is_def x = parse_def x
  | is_word x = parse_word x
  | otherwise = error "Unrecognized Expression"

-- We will want to parse the current

is_parens :: [Token] -> Bool
is_parens (x:xs) = x == LParen
is_parens _ = False

is_abs :: [Token] -> Bool
is_abs (x:xs) = x == Lambda
is_abs _ = False

is_word :: [Token] -> Bool
is_word ((Word _):xs) = True
is_word _ = False

is_def :: [Token] -> Bool
is_def ((Word x):Def:xs) = True
is_def _ = False

parse_def :: [Token] -> (Expr, [Token])
parse_def x =
  let (expr, rest) = parse.tail.tail $ x 
      var  = word_string.head $ x
  in ((Bind var expr), rest)

parse_word :: [Token] -> (Expr, [Token])
parse_word ((Word x):xs) = ((Var x), xs)

parse_parens :: [Token] -> (Expr, [Token])
parse_parens x
  | is_abs body = parse_abs body
  | is_parens body = parse_parens body
  | otherwise = parse_app body                       -- Assume it is an application if not an abstraction
  where out = get_parens_expr x
        body = fst out
        left = snd out

-- There isn't really a way to tell if a series of expressions is an application, instead of something else. implicitly, if there is a syntactic construct before another construct, then it is being applied to the first construc
parse_app :: [Token] -> (Expr, [Token])
parse_app _ = ((App (Var "x") (Var "x")),[])

-- This function takes a token string, and returns the sub token list that is contained in the outermost parens
-- The first token list returned by get_parens_expr should be the body of the parens, the second token list is the remaining tokens
get_parens_expr :: [Token] -> ([Token], [Token])
get_parens_expr (LParen:xs) = 
  let helper (x:xs) n
        | n == 0 = ([], x:xs)
        | x == LParen = let token_lists = helper xs (succ n) in (x: (fst token_lists), snd token_lists)
        | x == RParen && n == 1 = ([], xs)
        | x == RParen = let token_lists = helper xs (pred n) in (x: (fst token_lists), snd token_lists)
        | otherwise = let token_lists = helper xs n in (x:(fst token_lists), snd token_lists)
      helper [] n = if n == 0 then ([],[]) else error "Mismatched Parenthesis"
  in helper xs 1
get_parens_expr _ = error "Not a parenthesized expression"

parse_abs :: [Token] -> (Expr, [Token])
parse_abs x
  | valid_abs x = 
    let (expr, rest) = parse.tail.tail.tail $ x 
        var  = word_string.head.tail $ x
    in ((Abs (Var var) expr), rest)
  | otherwise = error ("Cannot Parse an invalidly formatted lambda expression.")

valid_abs :: [Token] -> Bool
valid_abs x = 
  let l = head $ x
      w = tail x
      d = head.tail.tail $ x
  in (l == Lambda) && (is_word w) && (d == Dot)

word_string :: Token -> String
word_string (Word x) = x
word_string _ = error "Tried to extract a string from not a word"

-- These functions are designed to parse very specific types of expressions
-- Takes an expression, returns the rest of the tokens after parsing the expression

-- Assume everything after this is just a definition. (so Assume parse_def was passed a definition)
--parse_def :: [Token] -> (Expr, [Token])

--parse_func :: [Token] -> (Expr, [Token])


--parse_abs :: [Token] -> (Expr, [Token])