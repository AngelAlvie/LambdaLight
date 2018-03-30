module Evaluator (
  Expr
, Comp
, Env
, Eval
, global_env
, lookup_prim
, eval
, apply
, substitute
, wrap
, var_val
, printer
, throw
, test
) where

import Data.HashMap.Strict as M
import Data.Hashable

-- we will define an environment of bindings, which we will execute our program in
type Env = HashMap String Expr

-- we will define 3 semantic constructs: a 'Var'iable, 'Abs'traction, and 'App'lication.
-- we will define 'Bind'ings, which modify the execution environment.
-- we will define 'Prim'atives, which point to built in functions and values, replaced at substitution time.
data Expr = Var String 
          | Abs Expr Expr 
          | App Expr Expr
          | Bind String Expr -- links a variable to an expression (attempt to simplify before binding)
          | Prim String deriving (Read, Eq)

global_env :: Env
global_env = empty

instance Show Expr where
  show (Var x) = x
  show (Abs x b) = "λ" ++ (show x) ++ "." ++ (show b)
  show (App e1 e2) =  "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
  show (Bind v e) = (show v) ++ " := " ++ (show e)
  show (Prim s) = "<Primitive: " ++ s ++ " >"

type Comp = (Expr, Env)

type Eval = Either String Comp

eval :: Comp -> Eval 
eval     (a@(Var v), env) = Right (a, env)
eval   (a@(Abs v b), env) = Right (a, env)
eval a@((App e1 e2), env) = apply a
eval    ((Bind s e), env) = eval (e, env) >>= \(x, _) -> Right (x, (insert s x env)) -- modifies current environment to add a new bindin
eval ((Prim s), env) = Right (lookup_prim s, env)

lookup_in_env :: String -> Env -> Maybe Expr
lookup_in_env var env = M.lookup var env

lookup_prim :: String -> Expr
lookup_prim _ = (Prim "None")

-- apply will evaluate variables as they are applied. So all variables will retain their names unless told otherwise.
apply :: Comp -> Eval
apply ((App e1@(Abs x b) e2), env) = (substitute x b e2 env) >>= eval
apply ((App e1@(Var x)   e2), env) = case (lookup_in_env x env) of Nothing   -> Left $ "No Binding exists for variable: " ++ (show e1)
                                                                   Just expr -> apply ((App expr e2), env)
apply ((App e1@(App a b) e2), env) = (eval (e1, env)) >>= \(exp, _) -> apply ((Abs exp e2), env)
apply (x, _) = Left $ "Internal Error: Attempted to Apply an expression that was not an application" ++ (show x)

substitute :: Expr -> Expr -> Expr -> Env -> Eval
substitute (Var name) body@(Var x) value env = if   (x == name)
                                               then Right (value, env)
                                               else Right (body, env)
-- Introduce local scoping, if two variables have same name, don't substitute subtree.
substitute (Var name) body@(Abs x b) value env = if   ((var_val x) == name)
                                                 then Right (body, env)
                                                 else wrap Abs (Right (x, env)) (substitute (Var name) b value env)  
substitute (Var name) body@(App x y) value env = wrap App (substitute (Var name) x value env) (substitute (Var name) y value env)
substitute bad@(Abs x y) _ _ _ = Left $ "Cannot bind abstractions with λ: " ++ (show bad)
substitute bad@(App x y) _ _ _ = Left $ "Cannot bind applications with λ: " ++ (show bad)


-- Wrap takes in a type constructor, and two evals (which may store an expression), and returns an Eval that wraps it (can be an abs or app)
wrap ::  (Expr -> Expr -> Expr) -> Eval -> Eval -> Eval
wrap tc (Right (e1, env)) (Right (e2, _)) = Right ((tc e1 e2), env) -- tc represents a type constructor -- this is skecthy
wrap tc x y = Left $ "Cannot wrap type constructor, there was an error:" ++ (show x) ++ " and " ++ (show y)

-- pulls out the name of a variable
var_val :: Expr -> String
var_val (Var name) = name

-- Prints the result of a computation to the screen
printer :: Eval -> IO ()
printer (Right (expr, _)) = putStrLn $ show expr
printer (Left err) = throw err

throw :: String -> IO ()
throw err = putStrLn $ ("ERROR: " ++ err)


test :: IO ()
test = do
  putStrLn "Testing apply"
  