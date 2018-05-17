-- This is an interpreter for the Lambda Calculus, written in Haskell
import System.Console.Haskeline
import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Evaluator
import Lexer
import Parser
import Common


-- Loop of the repl
main :: IO ()
main = getArgs >>= parse_args >>= repl

parse_args :: [String] -> IO Env
parse_args [] = return global_env
parse_args ["-h"] = usage >> exit
parse_args ["--help"] = usage >> exit
parse_args ["-v"] = version >> exit
parse_args ["--version"] = version >> exit
parse_args ["-l", file] = load_file file
parse_args ["--load", file] = load_file file
parse_args x = unknown >> exit

load_file :: String -> IO Env
load_file path = do
  contents <- readFile path
  putStrLn "Loading Defs.."
  let ans = (update_env (break_up_file contents))
  putStrLn "Defs Loaded"
  return ans

break_up_file :: String -> [String]
break_up_file str = (split '\n' str)

split ::  Char -> String -> [String]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s


update_env :: [String] -> Env
update_env strings = foldl change_env global_env strings

change_env :: Env -> String -> Env
change_env env str = case (eval_ str env) of Left err -> env
                                             Right (expr, env2) -> env2


usage = do 
  putStrLn "Usage: lc [-vhl] [file ..]" 
  putStrLn ""
  putStrLn "Options:" 
  putStrLn "-v, --version          Print Lambda Light version" 
  putStrLn "-h, --help             Bring up this help menu" 
  putStrLn "-l, --load [FILEPATH]  Load up a .lc file into current interpreter." 
  putStrLn "" 
  putStrLn "Current project version can be located at https://github.com/AngelAlvie/LambdaLight" 
version = putStrLn "lc v 1.0.0"
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)
unknown = putStrLn "Unkown flag, please use \"-h\" for help. "

-- This is where we will define the REPL

repl :: Env -> IO ()
repl env = runInputT defaultSettings (loop env)

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

eval_ :: String -> Env -> Eval
eval_ input env = (parse (tokenize input)) >>= \(ast, _) -> eval (ast, env)

quit_ :: String -> Bool
quit_ "quit" = True
quit_ "exit" = True
quit_ ":q"   = True
quit_ _      = False