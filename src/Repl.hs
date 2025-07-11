module Repl (repl) where
import Lexer (lexer)
import Parser (Expr(..), parser)
import Eval (Map, eval)

-- printExprs :: [Expr] -> IO ()
-- printExprs [] = ()
-- printExprs (x:xs) = do 
--   print x
--   printExprs xs

prettyPrint :: Expr -> String
prettyPrint (Atom x) = x
prettyPrint (Number x) = show x
prettyPrint (StringLit x) = x
prettyPrint (List x) = "(" ++ go x ++ ")"
  where
    go :: [Expr] -> String
    go [] = ""
    go [x] = prettyPrint x
    go (x:xs) = prettyPrint x ++ " " ++ go xs

repl :: Map -> IO ()
repl env = do
  putStr "TimiLisp> "
  input <- getLine
  let 
    tokens = lexer input
    expr = parser tokens [] !! 0
    (res, newEnv) = eval (expr, env)
  putStrLn $ prettyPrint res
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
