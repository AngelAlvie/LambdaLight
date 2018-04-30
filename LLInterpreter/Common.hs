-- This is a list of common functions that the Evaluator should have
-- Includes Type Declarations, helper methods, etc.

module Common (
  Token (..)
, Expr  (..)
, Comp  (..)
, Env   (..)
, Eval  (..)
, global_env
, lookup_prim
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
global_env = empty

instance Show Expr where
    show (Var x) = x
    show (Abs x b) = "λ" ++ (show x) ++ "." ++ (show b)
    show (App e1 e2) =  "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Bind v e) = (show v) ++ " := " ++ (show e)
    show (Prim s) = "<Primitive: " ++ s ++ ">"

lookup_in_env :: String -> Env -> Maybe Expr
lookup_in_env var env = M.lookup var env

lookup_prim :: String -> Expr
lookup_prim _ = (Prim "None")

type Comp = (Expr, Env)

type Eval = Either String Comp

-- Prints the result of a computation to the screen
printer :: Eval -> String
printer (Right (expr, _)) =show expr
printer (Left err) = throw err

throw :: String -> String
throw err = "ERROR: " ++ err
