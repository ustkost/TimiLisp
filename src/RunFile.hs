module RunFile where

import Data.List.Split (splitOn)

import Lexer (lexer)
import Parser (parser)
import Eval (eval, State)
import Parser (Expr(Error))

runFile :: FilePath -> IO ()
runFile name = do
  content <- readFile name
  let
    lines = splitOn "\n" content
  execute lines ([], [])
    where
      execute :: [String] -> State -> IO ()
      execute [] state = return ()
      execute (x:xs) state = do
        newState <- runLine x state
        execute xs newState

runLine :: String -> State -> IO (State)
runLine line state = do
  let
    tokens = lexer line 
    exprs = parser tokens []

  run exprs state
    where
      run :: [Expr] -> State -> IO (State)
      run [] state = return state
      run (x:xs) state = do
        (res, newState) <- eval x state
        case res of
          Error reason -> do
            print reason
            return state
          _ -> run xs newState
