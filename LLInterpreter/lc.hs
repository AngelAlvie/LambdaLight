-- This is an interpreter for the Lambda Calculus, written in Haskell
import System.Console.Haskeline
import Evaluator
import Lexer
import Parser
import Common
-- This is where we will define the REPL

eval_ :: String -> Env -> Eval
eval_ input env = (parse (tokenize input)) >>= \(ast, _) -> eval (ast, env)

-- Loop of the repl
main :: IO ()
main = runInputT defaultSettings (loop global_env)
  where
    loop :: Env -> InputT IO ()
    loop env = do
      input <- getInputLine "λ: "
      case input of
        Nothing -> return ()
        (Just str) -> if quit_ str
                      then return ()
                      else let 
                        result = eval_ str env
                        in do 
                          outputStrLn.printer $ result
                          case result of
                            Left s       -> do (loop env)
                            Right (e, n) -> do (loop n)

quit_ :: String -> Bool
quit_ "quit" = True
quit_ "exit" = True
quit_ ":q"   = True
quit_ _      = False