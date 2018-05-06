-- This is a list of common functions that the Evaluator should have
-- Includes Type Declarations, helper methods, etc.

module Common (
  Token (..)
, Expr  (..)
, Comp  (..)
, Env   (..)
, Eval  (..)
, global_env
, apply_primitive
, lookup_in_env
, printer
, throw
, insert
) where

import Data.HashMap.Strict as M

data Token = LParen | RParen | Word String | Lambda | Dot | Def | End deriving (Read, Show, Eq)

-- we will define 3 semantic constructs: a 'Var'iable, 'Abs'traction, and 'App'lication.
-- we will define 'Bind'ings, which modify the execution environment.
-- we will define 'Prim'atives, which point to built in functions and values, replaced at substitution time.
data Expr = Var String 
          | Abs Expr Expr 
          | App Expr Expr
          | Bind String Expr -- links a variable to an expression (attempt to simplify before binding)
          | Prim String deriving (Read, Eq)

-- we will define an environment of bindings, which we will execute our program in
type Env = HashMap String Expr

global_env :: Env
global_env = fromList [("show", (Prim "show"))]

instance Show Expr where
    show (Var x) = x
    show (Abs x b) = "λ" ++ (show x) ++ "." ++ (show b)
    show (App e1 e2) =  "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Bind v e) = (show v) ++ " := " ++ (show e)
    show (Prim s) = "<Primitive: " ++ s ++ ">"

lookup_in_env :: String -> Env -> Maybe Expr
lookup_in_env var env = M.lookup var env

apply_primitive :: String -> Expr -> Env -> Eval
apply_primitive "show" (Var s) env = case (M.lookup s env) of Nothing     -> Left $ "Unknown Function: " ++ s
                                                              (Just expr) -> Right (expr, env)
apply_primitive "show" expr env = Right (expr, env)
apply_primitive "None" x env = Right (x, env)
apply_primitive s _ _ = Left $ "Unknown Primitive: " ++ s

type Comp = (Expr, Env)

type Eval = Either String Comp


-- Prints the result of a computation to the screen
printer :: Eval -> String
printer (Right (expr, _)) =show expr
printer (Left err) = throw err

throw :: String -> String
throw err = "ERROR: " ++ err
