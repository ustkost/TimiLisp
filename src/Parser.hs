module Parser (parser, Expr(..)) where 

import Text.Read hiding (Number)

data Expr
  = Atom String
  | Number Integer
  | StringLit String
  | List [Expr]
  | Error String
  deriving (Show)

parser :: [String] -> [Expr] -> [Expr]
parser tokens oldExpr
  | tokens == [] = oldExpr
  | token == ")" = [Error "unpaired bracket"]
  | token == "(" = parser new_tokens (oldExpr ++ [lst])
  | otherwise = parser (tail tokens) (oldExpr ++ [convertedToken])
    where
      token = head tokens
      (lst, new_tokens) = processList (tail tokens) []
      isStringANumber :: String -> Bool
      isStringANumber s = (readMaybe s :: Maybe Integer) /= Nothing
      convertedToken 
        | isStringANumber token = Number (read token) 
        | (length token) > 0 && token !! 0 == '"' = StringLit (tail (init token))
        | otherwise = Atom token
      
      processList :: [String] -> [Expr] -> (Expr, [String])
      processList lst' e
        | lst' == [] = (Error "unpaired bracket in list", [])
        | x == ")" = (List e, xs)
        | otherwise = processList new_tokens' (e ++ [convertedToken2])
          where
            x:xs = lst'
            convertedToken2
              | x == "(" = fst (processList xs [])
              | isStringANumber x = Number (read x) 
              | x !! 0 == '"' = StringLit (tail (init x))
              | otherwise = Atom x
            new_tokens'
              | x == "(" = snd (processList xs [])
              | otherwise = xs
  
