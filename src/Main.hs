module Main (main) where

import Repl (repl)
import RunFile (runFile)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl ([], [])
    [filename] -> runFile filename
    _ -> putStrLn "Run without arguments for repl or pass file path to execute"
