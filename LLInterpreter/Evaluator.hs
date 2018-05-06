module Evaluator (
  eval
, apply
, substitute
, wrap
, var_val
) where

import Common

eval :: Comp -> Eval 
eval     (a@(Var v), env) = Right (a, env)
eval   (a@(Abs v b), env) = Right (a, env)
eval a@((App e1 e2), env) = apply a
eval    ((Bind s e), env) = if (is_combinator (e, env)) then eval (e, env) >>= \(x, _) -> Right (x, (insert s x env)) 
                                                     else Left $ "Expression: " ++ (show e) ++ " has variables that have not been defined yet"
eval ((Prim s), env) = Right (lookup_prim s, env)

-- apply will evaluate variables as they are applied. So all variables will retain their names unless told otherwise.
apply :: Comp -> Eval
apply ((App e1@(Abs x b) e2), env) = (substitute x b e2 env) >>= eval
apply ((App e1@(Var x)   e2), env) = case (lookup_in_env x env) of Nothing   -> Left $ "No Binding exists for variable: " ++ (show e1)
                                                                   Just expr -> apply ((App expr e2), env)
apply ((App e1@(App a b) e2), env) = (eval (e1, env)) >>= \(exp, _) -> apply ((App exp e2), env)
apply (x, _) = Left $ "Attempted to Apply an expression that was not an application: " ++ (show x)

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

-- this function checks to make sure that a particular expression is defined in terms of things that have already been defined.
is_combinator :: Comp -> Bool
is_combinator ((Abs (Var s) e), env) = is_combinator (e, (insert s (Var s) env))   
is_combinator     ((App e1 e2), env) = (is_combinator (e1, env)) && (is_combinator (e2, env))
is_combinator          ((Var v),env) = check_var (lookup_in_env v env)
is_combinator        ((Prim s), env) = check_var (lookup_in_env s env)
is_combinator      ((Bind s e), env) = is_combinator (e, env)

check_var :: Maybe Expr -> Bool
check_var Nothing     = False
check_var (Just expr) = True
