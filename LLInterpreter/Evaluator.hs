module Evaluator (
  Expr
, Eval
, eval
, apply
, substitute
, wrap
, var_val
, printer
, throw
, test
) where

import Data.Map.Strict 

-- we will define an environment of bindings, which we will execute our program in
type Env = Map String Expr

global_env :: Env
global_env = empty

data Expr = Var String 
          | Abs Expr Expr 
          | App Expr Expr
          | Bind String Expr
          | Prim String deriving (Read)
-- We will define 3 semantic constructs: a 'Var'iable, 'Abs'traction, and 'App'lication.
-- We will also define bindings, which modify the current environment,
-- and Prim, which is a primitive function, defined by the language

instance Show Expr where
  show (Var x) = x
  show (Abs x b) = "λ" ++ (show x) ++ "." ++ (show b)
  show (App e1 e2) =  "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
  show (Bind s e) = s ++ " := " ++ (show e)
  show (Prim s) = "<Built-In Function: " ++ s ++ " >"
type Eval = Either String Expr

eval :: Expr -> Eval
eval a@(Var x) = Right a
eval a@(Abs x b) = Right a
eval (App e1 e2) = apply e1 e2

apply :: Expr -> Expr -> Eval
apply e1@(Abs x b) e2 = (substitute x b e2) >>= eval
apply e1@(Var x) e2 = Left $ "Cannot apply an expression to a variable: " ++ (show e1)
apply e1@(App a b) e2 = (eval e1) >>= \x -> apply x e2

substitute :: Expr -> Expr -> Expr -> Eval
substitute (Var name) body@(Var x) value = if   (x == name)
                                           then Right value
                                           else Right body
-- Introduce local scoping, if two variables have same name, don't substitute subtree.
substitute (Var name) body@(Abs x b) value = if   ((var_val x) == name)
                                             then Right body
                                             else wrap Abs (Right x) (substitute (Var name) b value)  
substitute (Var name) body@(App x y) value =  wrap App (substitute (Var name) x value) (substitute (Var name) y value)
substitute bad@(Abs x y) _ _ = Left $ "Cannot bind abstractions with λ: " ++ (show bad)
substitute bad@(App x y) _ _ = Left $ "Cannot bind applications with λ: " ++ (show bad)

wrap ::  (Expr -> Expr -> Expr) -> Eval -> Eval -> Eval
wrap tc (Right e1) (Right e2) = Right (tc e1 e2) -- tc represents a type constructor
wrap _ _ _ = Left $ "Cannot bind within application"



var_val :: Expr -> String
var_val (Var name) = name

printer :: Eval -> IO ()
printer (Right expr) = putStrLn $ show expr
printer (Left err) = throw err

throw :: String -> IO ()
throw err = putStrLn $ ("ERROR: " ++ err)


test :: IO ()
test = do
  putStrLn "Testing apply"
  