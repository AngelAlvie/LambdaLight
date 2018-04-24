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
eval_ input = (parse.tokenize $ input)

print_ :: Eval -> IO ()
print_ = printer

-- Loop of the repl
main :: IO ()
main = do 
  input <- read_
  if (input == "quit")
  then return ()
  else
    result = eval_ input global
    
    print_(eval_ input) >> main