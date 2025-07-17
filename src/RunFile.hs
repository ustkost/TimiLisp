module RunFile where

import Lexer (lexer)
import Parser (parser)
import Eval (eval, State)

runFile :: FilePath -> IO ()
runFile name = do
  content <- readFile name
  let
    tokens = lexer input
    exprs = parser tokens []

  run exprs
	where
	  run :: [Expr] -> State -> State
    run [] state = state
    run (x:xs) state = run xs newState
      where
        (_, newState) = eval x state
