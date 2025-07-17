module Eval (eval, State, prettyPrint, exec) where

import Parser (Expr(..))
import System.Exit

type VarMap = [(String, Expr)]
type FuncMap = [(String, [String], [Expr])]
type State = (VarMap, FuncMap)
type OldVars = [(String, Maybe Expr)]

getVar :: VarMap -> String -> Maybe Expr
getVar vars name = lookup name vars
setVar :: VarMap -> String -> Expr -> VarMap
setVar [] name value = [(name, value)]
setVar (x:xs) name value
  | fst x == name = (name, value):xs
  | otherwise = x:setVar xs name value
delVar :: VarMap -> String -> VarMap
delVar [] _ = []
delVar ((vname, vvalue):vars) name
  | vname == name = vars
  | otherwise = (vname, vvalue):delVar vars name

getFunc :: FuncMap -> String -> Maybe ([String], [Expr])
getFunc [] _ = Nothing
getFunc ((fname, fargs, fbody):fs) name
  | fname == name = Just (fargs, fbody)
  | otherwise = getFunc fs name
setFunc :: FuncMap -> String -> [String] -> [Expr] -> FuncMap
setFunc [] name args body = [(name, args, body)]
setFunc ((a,b,c):fs) name args body
  | a == name = (name, args, body):fs
  | otherwise = (a,b,c):setFunc fs name args body

eval :: Expr -> State -> (Expr, State)
eval (Error x) state = (Error x, state)
eval (Atom "T") state = (Atom "T", state)
eval (Atom "nil") state = (Atom "nil", state)
eval (Atom vname) state = case getVar (fst state) vname of
  Just value -> (value, state)
  Nothing -> (Error ("variable " ++ vname ++ " is not defined"), state)
-- eval (Atom s) (vars, funcs) = (getVar vars s, (vars, funcs))
eval (Number n) state = (Number n, state)
eval (StringLit s) state = (StringLit s, state)
eval (List []) state = (Atom "nil", state)
eval (List ((Atom op):args)) state
  | op == "defun" && length args < 2 = (Error "defun requires at least 2 arguments: function name and its parameter list", state)
  | op == "defun" = let (func_name:params:body) = args
    in case func_name of
      Error reason -> (Error reason, state)
      Atom fname -> case params of
        Error reason -> (Error reason, state)
        List params_expr -> let getParamStrs :: [Expr] -> [String] -> Maybe [String]
                                getParamStrs [] names = Just names
                                getParamStrs (arg:args') names = case arg of
                                  Error reason -> Nothing
                                  Atom arg_name -> getParamStrs args' (names ++ [arg_name])
                                  _ -> Nothing
                                convd_args = getParamStrs params_expr []
                            in case convd_args of
                              Nothing -> (Error "incorrect parameter list", state)
                              Just names -> let (vars1, funcs1) = state
                                                state1 = (vars1, setFunc funcs1 fname names body)
                                            in (Atom fname, state1)
        _ -> (Error "lambda list is missed", state)
      _ -> (Error "the name of a function must be a symbol", state)
  | op == "makunbound" && length args /= 1 = (Error "makunbound accepts only 1 argument", state)
  | op == "makunbound" = let (evald_arg, state1) = eval (head args) state
    in case evald_arg of
      Error reason -> (Error reason, state)
      Atom name ->  let (vars1, funcs1) = state1
                        state2 = (delVar vars1 name, funcs1)
                    in (evald_arg, state2)
      _ -> (Error "makeunbound's argument should be a symbol", state)
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
  | op == "symbolp" && length args /= 1 = (Error "symbolp accepts only 1 argument", state)
  | op == "symbolp" = let (evald_arg, state1) = eval (head args) state
    in case evald_arg of
      Error reason -> (Error reason, state)
      Atom _ -> (Atom "T", state1)
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
                        StringLit str -> (Number (fromIntegral (length str)), state1)
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
  | op == "let" =
    let
      validateExprs :: [Expr] -> Bool
      validateExprs [] = True
      validateExprs ((List [Atom x, _]):xs) = validateExprs xs
      validateExprs (_:xs) = False
      
      validateStructure :: [Expr] -> Bool
      validateStructure [List exprs, _] = validateExprs exprs 
      validateStructure _ = False

      iterateExprs :: [Expr] -> OldVars -> VarMap -> (OldVars, VarMap)
      iterateExprs [] oldVars varMap = (oldVars, varMap)
      iterateExprs ((List [Atom x, y]):xs) oldVars varMap = 
        iterateExprs xs ((x, getVar varMap x) : oldVars) (setVar varMap x y)
      iterateExprs _ _ _ = ([], [])

      restoreVars :: OldVars -> VarMap -> VarMap
      restoreVars [] x = x
      restoreVars ((x, Just y):xs) m = restoreVars xs (setVar m x y)
      restoreVars ((x, Nothing):xs) m = restoreVars xs (delVar m x)

      (varMap, funcMap) = state
    in
      case validateStructure args of
        True ->
          let
            [List vars, expr] = args
            (oldVars, newVars) = iterateExprs vars [] varMap
            newState = (newVars, funcMap)
            (res, (evalState, _)) = eval expr newState
          in
            (res, (restoreVars oldVars evalState, funcMap))
        _ -> (Error "Usage: (let ((x 2) (y 3)) (+ x y))", state)
  | otherwise = callFunc op args state
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
      evalBody :: Expr -> [Expr] -> State -> (Expr, State)
      evalBody initexpr [] state1 = (initexpr, state1)
      evalBody initexpr (stmt:stmts) state1 = let (evald_stmt, state2) = eval stmt state1
        in case evald_stmt of
          Error reason -> (Error reason, state)
          _ -> evalBody evald_stmt stmts state2
      callFunc :: String -> [Expr] -> State -> (Expr, State)
      callFunc fname args' state1 = case getFunc (snd state1) fname of
        Nothing -> (Error ("undefined function " ++ show fname), state1)
        Just func ->  let (fargs, fbody) = func
          in if length fargs /= length args' then (Error ("function " ++ op ++ " requires " ++ show (length fargs) ++ " arguments"), state) else let
              saved_args = [getVar (fst state1) name | name <- fargs]
              setArgs :: [String] -> [Expr] -> State -> Maybe State
              setArgs [] [] state2 = Just state2
              setArgs (name:names) (value:values) state2 = let (evald_value, state3) = eval value state2
                in case evald_value of
                  Error reason -> Nothing
                  _ -> setArgs names values (setVar (fst state3) name evald_value, snd state3)
              tmpres = setArgs fargs args' state1
            in case tmpres of
              Nothing -> (Error "argument evaluating error", state)
              Just state2 ->  let (ret_value, state3) = evalBody (Atom "nil") fbody state2
                                  restoreArgs :: [String] -> [Maybe Expr] -> State -> State
                                  restoreArgs [] [] state4 = state4
                                  restoreArgs (name:names) (value:values) state4 = case value of
                                    Nothing -> (delVar (fst state4) name, snd state4)
                                    Just value -> (setVar (fst state4) name value, snd state4)
                                  state4 = restoreArgs fargs saved_args state3
                              in case ret_value of
                                Error reason -> (Error reason, state)
                                _ -> (ret_value, state4)
eval (List _) state = (Error "function name should be a symbol", state)
-- eval expr state = (Error ("WTF? How did you go here? expr = " ++ show expr), state)

-- EXEC
prettyPrint :: Expr -> String
prettyPrint (Atom x) = x
prettyPrint (Number x) = show x
prettyPrint (StringLit x) = x
prettyPrint (Error reason) = "Error: " ++ reason
prettyPrint (List elems) = "(" ++ go elems ++ ")"
  where
    go :: [Expr] -> String
    go [] = ""
    go [x] = prettyPrint x
    go (x:xs) = prettyPrint x ++ " " ++ go xs

exec :: Expr -> State -> IO (Expr, State)
exec (List [(Atom "print"), expr]) state = do
  print (prettyPrint expr)
  return (List [], state)
exec (List ((Atom "print"):_)) state = pure (Error "Usage: (print (1 2 3))", state)
exec other state = pure (eval other state)
