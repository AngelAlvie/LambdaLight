-- This is an interpreter for the Lambda Calculus, written in Haskell
import Control.Monad
import System.IO
import Evaluator
import Lexer
import Parser
import Common
-- This is where we will define the REPL


read_ :: IO String
read_ = putStr "λ: "
     >> hFlush stdout
     >> getLine

eval_ :: String -> Env -> Eval
eval_ input env = eval (fst (parse.tokenize $ input), env)


-- Loop of the repl
main :: IO ()
main = do
  main_helper global_env 
  return ()

main_helper :: Env -> IO ()
main_helper env = do
  input <- read_
  if (input == "quit")
  then return ()
  else let 
    result = eval_ input env
    in 
    case result of 
      Left s       -> printer result >> (main_helper env)
      Right (e, n) -> printer result >> (main_helper n)
