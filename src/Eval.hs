module Eval (eval, Map) where

import Parser (Expr(..))

type Map = [(String, Expr)]

getVar :: Map -> String -> Expr
getVar m name = 
  case lookup name m of
    (Just x) -> x
    (Nothing) -> error "Var not found"

setVar :: Map -> String -> Expr -> Map
setVar [] name value = [(name, value)]
setVar (x:xs) name value
  | fst x == name = (name, value) : xs
  | otherwise = x : setVar xs name value

eval :: (Expr, Map) -> (Expr, Map)
eval ((Atom "T"), m) = (Atom "T", m)
eval ((Atom "nil"), m) = (Atom "nil", m)
eval ((Atom s), m) = (getVar m s, m)
eval ((Number n), m) = (Number n, m)
eval ((StringLit s), m) = (StringLit s, m)
eval ((List []), m) = (Atom "nil", m)
eval ((List [(Atom "+"), lhs, rhs]), m) = (Number $ a + b, m)
  where
    (Number a, _) = eval (lhs, m)
    (Number b, _) = eval (rhs, m)
eval ((List [(Atom "-"), lhs, rhs]), m) = (Number $ a - b, m)
  where
    (Number a, _) = eval (lhs, m)
    (Number b, _) = eval (rhs, m)
eval ((List [(Atom "*"), lhs, rhs]), m) = (Number $ a * b, m)
  where
    (Number a, _) = eval (lhs, m)
    (Number b, _) = eval (rhs, m)
eval ((List [(Atom "/"), lhs, rhs]), m) = (Number (div a b), m)
  where
    (Number a, _) = eval (lhs, m)
    (Number b, _) = eval (rhs, m)
eval ((List [(Atom "setf"), (Atom name), value]), m) =
  let (evaluated, _) = eval (value, m)
  in (evaluated, setVar m name evaluated)
eval ((List [(Atom "atomp"), (Atom _)]), m) = (Atom "T", m)
eval ((List ((Atom "atomp"):_)), m) = (Atom "nil", m)

eval ((List [(Atom "numberp"), (Number _)]), m) = (Atom "T", m)
eval ((List ((Atom "numberp"):_)), m) = (Atom "nil", m)

eval ((List [(Atom "listp"), (List _)]), m) = (Atom "T", m)
eval ((List ((Atom "listp"):_)), m) = (Atom "nil", m)

eval ((List [(Atom "car"), expr]), m) = (head e, newM)
  where
    ((List e), newM) = eval (expr, m)

eval ((List [(Atom "cdr"), expr]), m) = (List (tail e), newM)
  where
    ((List e), newM) = eval (expr, m)

eval ((List [(Atom "="), lhs, rhs]), m) = (go (eval (lhs, m)) (eval (rhs, m)), m)
  where
    go (Number a, _) (Number b, _)
      | a == b = Atom "T"
      | otherwise = Atom "nil"
    go _ _ = Atom "nil"

eval ((List [(Atom "quote"), x]), m) = (x, m)

eval (List ((Atom "list"):xs), m) = (List (go xs), m)
  where
    go :: [Expr] -> [Expr]
    go [] = []
    go (x:xs) = fst (eval (x, m)) : go xs

eval (List [(Atom "cons"), lhs, rhs], m) = (go (eval (lhs, m)) (eval (rhs, m)), m)
  where
    go :: (Expr, Map) -> (Expr, Map) -> Expr
    go (a, _) (List b, _) = List (a:b)
