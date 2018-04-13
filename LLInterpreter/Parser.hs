module Parser (
    parse
) where

import Lexer    -- This is just: LParen | RParen | Word String | Lambda | Dot | Def
import Evaluator -- This is just: Var String | Abs Expr Expr | App Expr Expr | Bind String Expr | Prim String

-- The Parser should take in a list of tokens, and construct an Abstract Syntax Tree based on the Lexs that are returned by the lexer,
-- The AST is defined in terms of the Evaluator Expr type.

parse :: [Token] -> Maybe Expr
parse [] = Nothing
parse (x:xs) = case x of x -> Nothing

parse_prev :: Maybe Expr -> [Token] -> Maybe Expr
parse_prev Nothing [] = Nothing
parse_prev Nothing (x:xs) = case x of Word s -> parse_prev (Just (Var s)) xs
                                      LParen -> Nothing-- check if abstraction or application
                                      RParen -> Nothing-- check if accompanying paren
                                      Lambda -> Nothing-- denotes an abstraction, we know we are seeing a variable
                                      Dot    -> Nothing-- denotes a body
                                      Def    -> Nothing-- Error, no prev_expression
parse_prev (Just e) [] = e
parse_prev (Just e) (x:xs) = case x of Word s -> parse_prev (Just (Var s)) xs
                                       LParen -> Nothing-- check if abstraction or application
                                       RParen -> Nothing-- check if accompanying paren
                                       Lambda -> Just (Abs )-- denotes an abstraction, we know we are seeing a variable
                                       Dot    -> Nothing-- denotes a body
                                       Def    -> Just (Bind e (parse_prev e xs)) -- denotes a binding

--This takes a token list and returns the next thing in the sequence, otherwise return an empty list
next_token :: [Token] -> Token
next_token [] = []
next_token (x:xs) = head xs

-- parse parens takes a list of tokens and returns the expression that it parses
--parse_parens :: [Token] ->  (Expr, [Token])
--parse_parens 
