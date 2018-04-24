module Parser (
    parse
) where

import Common

-- The Parser should take in a list of tokens, and construct an Abstract Syntax Tree based on the Lexs that are returned by the lexer,
-- The AST is defined in terms of the Evaluator Expr type.

-- The parse function is just a dispatcher that checks if the token list implies a particular type of expression,
-- And calls the appropriate sub-parser to handle it

parse :: [Token] -> (Expr, [Token])
parse x
  | is_parens x = parse_app.parse_parens $ x
  | is_abs x = parse_app.parse_abs $ x
  | is_def x = parse_app.parse_def $ x
  | is_word x = parse_app.parse_word $ x
  | otherwise = error "Unrecognized Expression"

-- We want to check whether or not we have leftover expressions, 
-- if we do, then parse them and create an application with my current Expression
parse_app :: (Expr, [Token]) -> (Expr, [Token])
parse_app (e, []) = (e, [])
parse_app (e, y) = let x = parse y in ((App e (fst x)), (snd x))


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
  | is_abs body    = ((fst (parse body)), right)
  | is_parens body = ((fst (parse body)), right)
  | is_word body   = ((fst (parse body)), right)
  | otherwise = error "Cannot parse statement"               -- Assume it is an application if not an abstraction
  where out = get_parens_expr x
        body = fst out
        right = snd out

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
