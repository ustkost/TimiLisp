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
  execute lines ([], []) 1
    where
      -- Integer потому что у нас амбициозный интерпрeтатор (от Искандера)
      execute :: [String] -> State -> Integer -> IO ()
      execute [] state num = return ()
      execute (x:xs) state num = do
        newState <- runLine x state num
        case newState of
          Just newState -> execute xs newState (num + 1)
          Nothing -> return ()

runLine :: String -> State -> Integer -> IO (Maybe State)
runLine line state num = do
  let
    tokens = lexer line 
    exprs = parser tokens []

  run exprs state
    where
      run :: [Expr] -> State -> IO (Maybe State)
      run [] state = return (Just state)
      run (x:xs) state = do
        (res, newState) <- eval x state
        case res of
          Error reason -> do
            print ("Line " ++ show num ++ ": " ++ reason)
            return Nothing 
          _ -> run xs newState
