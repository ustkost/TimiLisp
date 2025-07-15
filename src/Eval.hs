module Eval (eval, State) where

import Parser (Expr(..))
import System.Exit

type VarMap = [(String, Expr)]
type FuncMap = [(String, [String], Expr)]
type State = (VarMap, FuncMap)

getVar :: VarMap -> String -> Expr
getVar vars name = 
  case lookup name vars of
    (Just x) -> x
    (Nothing) -> Error ("variable " ++ name ++ " is not found")

setVar :: VarMap -> String -> Expr -> VarMap
setVar [] name value = [(name, value)]
setVar (x:xs) name value
  | fst x == name = (name, value):xs
  | otherwise = x:setVar xs name value

setFunc :: FuncMap -> String -> [String] -> Expr -> FuncMap
setFunc [] name args body = [(name, args, body)]
setFunc ((a,b,c):fs) name args body
  | a == name = (name, args, body):fs
  | otherwise = (a,b,c):setFunc fs name args body

eval :: Expr -> State -> (Expr, State)
eval (Error x) state = (Error x, state)
eval (Atom "t") state = (Atom "t", state)
eval (Atom "nil") state = (Atom "nil", state)
eval (Atom s) (vars, funcs) = (getVar vars s, (vars, funcs))
eval (Number n) state = (Number n, state)
eval (StringLit s) state = (StringLit s, state)
eval (List []) state = (Atom "nil", state)
eval (List ((Atom op):args)) state
  {-| op == "exit" && length args == 0 = do
    exitWith (ExitSuccess)
    return (Atom "WTF", vars)
  | op == "exit" && length args == 1 = let (evald_arg, vars1) = eval (head args) vars
    in case evald_arg of
      Error reason -> (Error reason, vars)
      Number exitCode -> do
        exitWith (ExitFailure (fromIntegral exitCode))
        return (Atom "WTF", vars1)
      _ -> (Error "exit accepts only 1 numeric argument", vars)
  | op == "defun" = let
    in
  | op == "exit" && length args >= 2 = (Error "exit accepts only 1 numeric argument", vars)-}
  | op == "+" = let sumNums :: [Expr] -> State -> Integer -> (Expr, State)
                    sumNums [] state1 acc = (Number acc, state1)
                    sumNums (expr:exprs) state1 acc = let (evald_expr, state2) = eval expr state1
                                                  in case evald_expr of
                                                    (Error reason) -> (Error reason, state)
                                                    (Number n) -> sumNums exprs state2 (acc + n)
                                                    _ -> (Error "+'s argument is not a number", state)
                in sumNums args state 0
  | op == "*" = let mulNums :: [Expr] -> State -> Integer -> (Expr, State)
                    mulNums [] state1 acc = (Number acc, state1)
                    mulNums (expr:exprs) state1 acc = let (evald_expr, state2) = eval expr state1
                                                  in case evald_expr of
                                                    (Error reason) -> (Error reason, state)
                                                    (Number n) -> mulNums exprs state2 (acc * n)
                                                    _ -> (Error ("*'s argument is not a number: " ++ show evald_expr), state)
                in mulNums args state 1
  | op == "-" && length args == 0 = (Error "too few arguments to -", state)
  | op == "-" && length args == 1 = let (evald_arg, state1) = eval (head args) state
                                    in case evald_arg of
                                      Error reason -> (Error reason, state)
                                      Number n -> (Number (-n), state1)
                                      _ -> (Error "- works only with numbers", state)
  | op == "-" = let (evald_arg1, state1) = eval (head args) state
    in case evald_arg1 of
      Error reason -> (Error reason, state)
      Number n -> let go :: Integer -> [Expr] -> State -> (Expr, State)
                      go acc [] state1 = (Number acc, state1)
                      go acc (arg:args') state1 = let (evald_arg, state2) = eval arg state1
                        in case evald_arg of
                          Error reason -> (Error reason, state)
                          Number subt -> go (acc - subt) args' state2
                          _ -> (Error "- works only with numbers", state)
                  in go n (tail args) state1
      _ -> (Error "- works only with numbers", state)
  | (op == "=" || op == ">=" || op == "<=" || op == "/=" || op == "<" || op == ">") && length args == 0 = (Error "too few argument given to =", state)
  | op == "=" = arithmeticPredicate (==) args state
  | op == ">=" = arithmeticPredicate (>=) args state
  | op == ">" = arithmeticPredicate (>) args state
  | op == "<" = arithmeticPredicate (<) args state
  | op == "<=" = arithmeticPredicate (<=) args state
  | op == "/=" = arithmeticPredicate (/=) args state
  | op == "and" = let andExpr :: Expr -> [Expr] -> State -> (Expr, State)
                      andExpr e [] state1 = (e, state1)
                      andExpr _ (arg:args') state1 =  let (evald_arg, state2) = eval arg state1
                                                    in case evald_arg of
                                                      Error reason -> (Error reason, state)
                                                      Atom "nil" -> (Atom "nil", state2)
                                                      _ -> andExpr evald_arg args' state2
                  in andExpr (Atom "T") args state
  | op == "or" =  let orExpr :: Expr -> [Expr] -> State -> (Expr, State)
                      orExpr e [] state1 = (e, state1)
                      orExpr _ (arg:args') state1 = let (evald_arg, state2) = eval arg state1
                                                  in case evald_arg of
                                                    Error reason -> (Error reason, state)
                                                    Atom "nil" -> orExpr evald_arg args' state2
                                                    _ -> (evald_arg, state2)
                  in orExpr (Atom "nil") args state
  | op == "cond" = let  condExpr :: [Expr] -> State -> (Expr, State)
                        condExpr [] state1 = (Atom "nil", state1)
                        condExpr (cond_list:cond_lists) state1 = case cond_list of
                          Error reason -> (Error reason, state)
                          List [] -> (Error "there is no condition in a cond block", state)
                          List (cond_expr:cond_stmts) -> let (evald_cond, state2) = eval cond_expr state1
                            in case evald_cond of
                              Error reason -> (Error reason, state)
                              (Atom "nil") -> condExpr cond_lists state2
                              _ ->  let runStmts :: [Expr] -> State -> (Expr, State)
                                        runStmts [] state3 = (evald_cond, state3)
                                        runStmts [stmt] state3 = let (evald_stmt, state4) = eval stmt state
                                          in case evald_stmt of
                                            Error reason -> (Error reason, state)
                                            _ -> (evald_stmt, state4)
                                        runStmts (stmt:stmts) state3 = let (evald_stmt, state4) = eval stmt state3
                                          in case evald_stmt of
                                            Error reason -> (Error reason, state)
                                            _ -> runStmts stmts state4
                                    in runStmts cond_stmts state2
                          _ -> (Error "cond arguments should be lists", state)
    in condExpr args state
  | op == "setf" && odd (length args) = (Error "the number of setf's arguments must be odd", state)
  | op == "setf" =  let
                      setVars :: [Expr] -> State -> (Expr, State)
                      setVars [] state1 = (Atom "nil", state1)
                      setVars [name, value] state1 = let (evaluated_value, state2) = eval value state1
                        in case evaluated_value of
                          (Error x) -> (Error x, state)
                          _ -> case name of
                            (Atom aname) -> (evaluated_value, ((setVar (fst state2) aname evaluated_value), snd state2))
                            _ -> (Error "name must be an atom", state)
                      setVars (name:value:other) state1 =
                        let (v2, state2) = setVars [name, value] state1
                        in case v2 of
                          Error x -> (Error x, state)
                          _ -> setVars other state2
                    in setVars args state
  | op == "atom" && length args /= 1 = (Error "atom accepts only 1 argument", state)
  | op == "atom" =  let (evald_arg, state1) = eval (head args) state
                    in case evald_arg of
                      Error reason -> (Error reason, state)
                      List _ -> (Atom "nil", state1)
                      _ -> (Atom "T", state1)
  | op == "numberp" && length args /= 1 = (Error "numberp accepts only 1 argument", state)
  | op == "numberp" = let (evald_arg, state1) = eval (head args) state
                      in case evald_arg of
                        Error reason -> (Error reason, state)
                        Number _ -> (Atom "T", state1)
                        _ -> (Atom "nil", state1)
  | op == "listp" && length args /= 1 = (Error "listp accepts only 1 argument", state)
  | op == "listp" = let (evald_arg, state1) = eval (head args) state
                    in case evald_arg of
                      Error reason -> (Error reason, state)
                      List _ -> (Atom "T", state1)
                      _ -> (Atom "nil", state1)
  | op == "null" && length args /= 1 = (Error "null accepts only 1 argument", state)
  | op == "null" =  let (evald_arg, state1) = eval (head args) state
                    in case evald_arg of
                      Error reason -> (Error reason, state)
                      Atom "nil" -> (Atom "T", state1)
                      _ -> (Atom "nil", state1)
  | op == "quote" && length args /= 1 = (Error "quote accepts only 1 argument", state)
  | op == "quote" = let arg = head args
                    in case arg of
                      Error reason -> (Error reason, state)
                      _ -> (arg, state)
  | op == "list" =  let joinArgs :: [Expr] -> State -> Expr -> (Expr, State)
                        joinArgs [] state1 lst = (lst, state1)
                        joinArgs (arg:args') state1 (List lst) =  let (evald_arg, state2) = eval arg state1
                                                                in case evald_arg of
                                                                  Error reason -> (Error reason, state)
                                                                  _ -> joinArgs args' state2 (List (lst ++ [evald_arg]))
                    in joinArgs args state (List [])
  | op == "car" && length args /= 1 = (Error "car accepts only 1 argument", state)
  | op == "car" = let (evald_arg, state1) = eval (head args) state
                  in case evald_arg of
                    Error reason -> (Error reason, state)
                    Atom "nil" -> (Atom "nil", state1)
                    List lst -> (head lst, state1)
                    _ -> (Error "the argument of car shall be a list", state)
  | op == "cdr" && length args /= 1 = (Error "cdr accepts only 1 argument", state)
  | op == "cdr" = let (evald_arg, state1) = eval (head args) state
                  in case evald_arg of
                    Error reason -> (Error reason, state)
                    Atom "nil" -> (Atom "nil", state1)
                    List lst -> (List (tail lst), state1)
                    _ -> (Error "the argument of cdr shall be a list", state)
  | op == "cons" && length args /= 2 = (Error "cons accepts only 2 arguments", state)
  | op == "cons" =  let [arg1, arg2] = args
                        (evald_arg1, state1) = eval arg1 state
                    in case evald_arg1 of
                      Error reason -> (Error reason, state)
                      _ ->  let (evald_arg2, state2) = eval arg2 state1
                            in case evald_arg2 of
                              Error reason -> (Error reason, state)
                              (List lst) -> (List (evald_arg1:lst), state2)
                              _ -> (Error "the second argument of cons shall be a list", state)
  | op == "length" && length args /= 1 = (Error "length accepts only 1 argument", state)
  | op == "length" =  let (evald_arg, state1) = eval (head args) state
                      in case evald_arg of
                        Error reason -> (Error reason, state)
                        Atom "nil" -> (Number 0, state1)
                        List lst -> (Number (fromIntegral (length lst)), state1)
                        _ -> (Error "the argument of length shall be a list", state)
  | op == "equal" && length args /= 2 = (Error "equals accepts only 2 arguments", state)
  | op == "equal" = let [arg1, arg2] = args
                        (evald_arg1, state1) = eval arg1 state
                    in case evald_arg1 of
                      Error reason -> (Error reason, state)
                      _ ->  let (evald_arg2, state2) = eval arg2 state1
                            in case evald_arg2 of
                              Error reason -> (Error reason, state)
                              _ ->  let exprEquals :: Expr -> Expr -> Bool
                                        exprEquals (Atom a) (Atom b) = a == b
                                        exprEquals (Number a) (Number b) = a == b
                                        exprEquals (StringLit a) (StringLit b) = a == b
                                        exprEquals (List a) (List b) =  let f :: [Expr] -> [Expr] -> Bool
                                                                            f (x:xs) (y:ys) = if exprEquals x y then f xs ys else False
                                                                        in length a == length b && f a b
                                        exprEquals _ _ = False
                                                    in (Atom (if exprEquals evald_arg1 evald_arg2 then "T" else "nil"), state2)
  | op == "error" && length args /= 1 = (Error "error accepts only 1 argument", state)
  | op == "error" = let (evald_arg, state1) = eval (head args) state
                    in case evald_arg of
                      Error reason -> (Error reason, state)
                      StringLit str -> (Error str, state1)
                      _ -> (Error "the argument of error shall bet a string", state)
  | op == "eval" && length args /= 1 = (Error "eval accepts only 1 argument", state)
  | op == "eval" =  let (evald_arg, state1) = eval (head args) state
                    in case evald_arg of
                      Error reason -> (Error reason, state)
                      _ -> eval evald_arg state1
  | otherwise = (Error "incorrect operation", state)
    where
      arithmeticPredicate :: (Integer -> Integer -> Bool) -> [Expr] -> State -> (Expr, State)
      arithmeticPredicate _ [] state1 = (Error "arithmetic predicates require at least 1 argument", state1)
      arithmeticPredicate f (arg1:args') state1 = let (evald_arg1, state2) = eval (head args) state
        in case evald_arg1 of
          Error reason -> (Error reason, state)
          Number m -> let isBE :: Integer -> [Expr] -> State -> (Expr, State)
                          isBE _ [] state3 = (Atom "T", state3)
                          isBE arg1' (arg2:args') state3 = let (evald_arg2, state4) = eval arg2 state3
                            in case evald_arg2 of
                              Error reason -> (Error reason, state1)
                              Number n -> if f arg1' n then isBE n args' state4 else (Atom "nil", state4)
                              _ -> (Error "arithmetic predicates accept only numerical arguments", state1)
            in isBE m (tail args) state2
          _ -> (Error "arithmetic predicates accept only numerical arguments", state1)
eval expr state = (Error ("WTF? How did you go here? expr = " ++ show expr), state)
