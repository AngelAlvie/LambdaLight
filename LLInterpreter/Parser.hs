module Parser (
    parse
) where

import Common

-- The Parser should take in a list of tokens, and construct an Abstract Syntax Tree based on the Lexs that are returned by the lexer,
-- The AST is defined in terms of the Evaluator Expr type.

-- The parse function is just a dispatcher that checks if the token list implies a particular type of expression,
-- And calls the appropriate sub-parser to handle it

parse :: [Token] -> Either String (Expr, [Token])
parse x = parse_app.parse_atomic $ x
  
parse_app :: Either String (Expr, [Token]) -> Either String (Expr, [Token])
parse_app (Left s) = Left s
parse_app (Right (first, rest)) = if   null rest
                                then Right (first, [])
                                else parse_atomic rest >>= \(next, rnext) -> parse_app (Right ((App (first) (next)), rnext))

-- We want to check whether or not we have leftover expressions, 
-- if we do, then parse them and create an application with my current Expression
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

-- Dispatcher Method
parse_atomic :: [Token] -> Either String (Expr, [Token])
parse_atomic x
  | is_def x    = parse_def $ x -- a definition can never be applied.
  | is_parens x = parse_parens $ x -- A parenthesized expression 
  | is_abs x    = parse_abs $ x 
  | is_word x   = parse_word $ x 
  | null x      = Right ((Prim "None"), [])
  | otherwise   = Left "Unrecognized Expression"

parse_def :: [Token] -> Either String (Expr, [Token])
parse_def (var:_:def) = parse def >>= \(expr, rest) -> Right ((Bind (word_string var) expr), rest)

parse_word :: [Token] -> Either String (Expr, [Token])
parse_word ((Word x):xs) = Right ((Var x), xs)

parse_parens :: [Token] -> Either String (Expr, [Token])
parse_parens x = get_parens_expr x >>= \(body, right) -> if   is_abs body || is_parens body || is_word body 
                                                         then (parse body) >>= \(expr, rest) -> Right (expr, right)
                                                         else Left "Cannot parse parenthesized statement"
  
-- This function takes a token string, and returns the sub token list that is contained in the outermost parens
-- The first token list returned by get_parens_expr should be the body of the parens, the second token list is the remaining tokens
helper :: [Token] -> Int -> Either String ([Token], [Token])
helper [] n = if n == 0 then Right ([],[]) else Left "Mismatched Parenthesis"
helper (x:xs) n
  | n == 0 = Right ([], x:xs)
  | x == LParen = helper xs (succ n) >>= \token_lists -> Right (x: (fst token_lists), snd token_lists)
  | x == RParen && n == 1 = Right ([], xs)
  | x == RParen = helper xs (pred n) >>= \token_lists -> Right (x: (fst token_lists), snd token_lists)
  | otherwise   = helper xs n        >>= \token_lists -> Right (x:(fst token_lists), snd token_lists)


get_parens_expr :: [Token] -> Either String ([Token], [Token])
get_parens_expr (LParen:xs) = helper xs 1
get_parens_expr _ = Left "Not a parenthesized expression"

parse_abs :: [Token] -> Either String (Expr, [Token])
parse_abs (Lambda:x:Dot:xs) = parse xs >>= \(expr, rest) -> if is_word [x] 
                                                            then Right ((Abs (Var (word_string x)) expr), rest)
                                                            else Left "Tried to create an abstraction of something that isn't an abstraction"
parse_abs _ = Left "Cannot Parse an invalidly formatted lambda expression."

word_string :: Token -> String
word_string (Word x) = x