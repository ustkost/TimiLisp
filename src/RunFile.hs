module RunFile where

import Lexer (lexer)
import Parser (parser)
import Eval (eval, State)
import Parser (Expr)

runFile :: FilePath -> IO ()
runFile name = do
  content <- readFile name
  let
    tokens = lexer content 
    exprs = parser tokens []

  _ <- run exprs ([], [])
  return ()
    where
      run :: [Expr] -> State -> IO (State)
      run [] state = return state
      run (x:xs) state = do
        (_, newState) <- eval x state
        run xs newState
