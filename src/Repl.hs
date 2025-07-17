module Repl (repl) where
import Lexer (lexer)
import Parser (Expr(..), parser)
import Eval (eval, State, prettyPrint)

-- printExprs :: [Expr] -> IO ()
-- printExprs [] = ()
-- printExprs (x:xs) = do 
--   print x
--   printExprs xs

repl :: State -> IO ()
repl env = do
  putStr "TimiLisp> "
  input <- getLine
  let 
    tokens = lexer input
    expr = parser tokens [] !! 0
  (res, newEnv) <- eval expr env
  putStrLn (prettyPrint res)
  repl newEnv

-- repl :: Map -> IO Map
-- repl = do
--   putStr "TimiLisp> "
--   input <- getLine
-- 	putStrLn res
-- 	repl m
-- 	  where 
--       tokens = lexer input
--       expr = parser tokens
--       (res, m) = eval expr
