module Eval (eval, State, prettyPrint) where

import Parser (Expr(..))
import System.Exit
import Control.Monad (foldM)

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

eval :: Expr -> State -> IO (Expr, State)
eval (Error x) state = return (Error x, state)
eval (Atom "T") state = return (Atom "T", state)
eval (Atom "nil") state = return (Atom "nil", state)
eval (Atom vname) state = return $ case getVar (fst state) vname of
  Just value -> (value, state)
  Nothing -> (Error ("variable " ++ vname ++ " is not defined"), state)
eval (Number n) state = return (Number n, state)
eval (StringLit s) state = return (StringLit s, state)
eval (List []) state = return (Atom "nil", state)
eval (List ((Atom op):args)) state
  | op == "defun" && length args < 2 = return (Error "defun requires at least 2 arguments: function name and its parameter list", state)
  | op == "defun" = let (func_name:params:body) = args
    in case func_name of
      Error reason -> return (Error reason, state)
      Atom fname -> case params of
        Error reason -> return (Error reason, state)
        List params_expr -> let getParamStrs :: [Expr] -> [String] -> Maybe [String]
                                getParamStrs [] names = Just names
                                getParamStrs (arg:args') names = case arg of
                                  Error reason -> Nothing
                                  Atom arg_name -> getParamStrs args' (names ++ [arg_name])
                                  _ -> Nothing
                                convd_args = getParamStrs params_expr []
                            in case convd_args of
                              Nothing -> return (Error "incorrect parameter list", state)
                              Just names -> let (vars1, funcs1) = state
                                                state1 = (vars1, setFunc funcs1 fname names body)
                                            in return (Atom fname, state1)
        _ -> return (Error "lambda list is missed", state)
      _ -> return (Error "the name of a function must be a symbol", state)
  | op == "apply" && length args /= 2 = return (Error "apply requires exactly 2 arguments", state)
  | op == "apply" = do
        (evald_fname, state1) <- eval (args !! 0) state
        case evald_fname of
          Error reason -> return (Error reason, state1)
          Atom fname -> do
            (evald_fargs, state2) <- eval (args !! 1) state1
            case evald_fargs of
              Error reason -> return (Error reason, state2)
              List fargs -> callFunc fname fargs state2
              _ -> return (Error "function arguments in apply should be a list", state2)
          _ -> return (Error "function name in apply should be a symbol", state1)
  | op == "makunbound" && length args /= 1 = return (Error "makunbound accepts only 1 argument", state)
  | op == "makunbound" = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
          Atom name ->  let (vars1, funcs1) = state1
                            state2 = (delVar vars1 name, funcs1)
                        in return (evald_arg, state2)
          _ -> return (Error "makeunbound's argument should be a symbol", state1)
  | op == "+" = do
        let sumNums [] state1 acc = return (Number acc, state1)
            sumNums (expr:exprs) state1 acc = do
                (evald_expr, state2) <- eval expr state1
                case evald_expr of
                  (Error reason) -> return (Error reason, state2)
                  (Number n) -> sumNums exprs state2 (acc + n)
                  _ -> return (Error "+'s argument is not a number", state2)
        sumNums args state 0
  | op == "*" = do
        let mulNums [] state1 acc = return (Number acc, state1)
            mulNums (expr:exprs) state1 acc = do
                (evald_expr, state2) <- eval expr state1
                case evald_expr of
                  (Error reason) -> return (Error reason, state2)
                  (Number n) -> mulNums exprs state2 (acc * n)
                  _ -> return (Error ("*'s argument is not a number: " ++ show evald_expr), state2)
        mulNums args state 1
  | op == "-" && length args == 0 = return (Error "too few arguments to -", state)
  | op == "-" && length args == 1 = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
          Number n -> return (Number (-n), state1)
          _ -> return (Error "- works only with numbers", state1)
  | op == "-" = do
        (evald_arg1, state1) <- eval (head args) state
        case evald_arg1 of
          Error reason -> return (Error reason, state1)
          Number n -> do
            let go :: Integer -> [Expr] -> State -> IO (Expr, State)
                go acc [] state1 = return (Number acc, state1)
                go acc (arg:args') state1 = do
                  (evald_arg, state2) <- eval arg state1
                  case evald_arg of
                    Error reason -> return (Error reason, state2)
                    Number subt -> go (acc - subt) args' state2
                    _ -> return (Error "- works only with numbers", state2)
            go n (tail args) state1
          _ -> return (Error "- works only with numbers", state1)
  | (op == "=" || op == ">=" || op == "<=" || op == "/=" || op == "<" || op == ">") && length args == 0 = return (Error "too few argument given to =", state)
  | op == "=" = arithmeticPredicate (==) args state
  | op == ">=" = arithmeticPredicate (>=) args state
  | op == ">" = arithmeticPredicate (>) args state
  | op == "<" = arithmeticPredicate (<) args state
  | op == "<=" = arithmeticPredicate (<=) args state
  | op == "/=" = arithmeticPredicate (/=) args state
  | op == "and" = do
        let andExpr e [] state1 = return (e, state1)
            andExpr _ (arg:args') state1 = do
                (evald_arg, state2) <- eval arg state1
                case evald_arg of
                  Error reason -> return (Error reason, state2)
                  Atom "nil" -> return (Atom "nil", state2)
                  _ -> andExpr evald_arg args' state2
        andExpr (Atom "T") args state
  | op == "or" = do
        let orExpr e [] state1 = return (e, state1)
            orExpr _ (arg:args') state1 = do
                (evald_arg, state2) <- eval arg state1
                case evald_arg of
                  Error reason -> return (Error reason, state2)
                  Atom "nil" -> orExpr evald_arg args' state2
                  _ -> return (evald_arg, state2)
        orExpr (Atom "nil") args state
  | op == "cond" = do
        let condExpr [] state1 = return (Atom "nil", state1)
            condExpr (cond_list:cond_lists) state1 = case cond_list of
              Error reason -> return (Error reason, state1)
              List [] -> return (Error "there is no condition in a cond block", state1)
              List (cond_expr:cond_stmts) -> do
                  (evald_cond, state2) <- eval cond_expr state1
                  case evald_cond of
                    Error reason -> return (Error reason, state2)
                    (Atom "nil") -> condExpr cond_lists state2
                    _ -> evalBody evald_cond cond_stmts state2
              _ -> return (Error "cond arguments should be lists", state1)
        condExpr args state
  | op == "setf" && odd (length args) = return (Error "the number of setf's arguments must be odd", state)
  | op == "setf" = do
        let setVars :: [Expr] -> State -> IO (Expr, State)
            setVars [] state1 = return (Atom "nil", state1)
            setVars [name, value] state1 = do
                (evaluated_value, state2) <- eval value state1
                case evaluated_value of
                  (Error x) -> return (Error x, state2)
                  _ -> case name of
                    (Atom aname) -> return (evaluated_value, (setVar (fst state2) aname evaluated_value, snd state2))
                    _ -> return (Error "name must be an atom", state2)
            setVars (name:value:other) state1 = do
                (v2, state2) <- setVars [name, value] state1
                case v2 of
                  Error x -> return (Error x, state2)
                  _ -> setVars other state2
        setVars args state
  | op == "atom" && length args /= 1 = return (Error "atom accepts only 1 argument", state)
  | op == "atom" = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
          List _ -> return (Atom "nil", state1)
          _ -> return (Atom "T", state1)
  | op == "numberp" && length args /= 1 = return (Error "numberp accepts only 1 argument", state)
  | op == "numberp" = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
          Number _ -> return (Atom "T", state1)
          _ -> return (Atom "nil", state1)
  | op == "listp" && length args /= 1 = return (Error "listp accepts only 1 argument", state)
  | op == "listp" = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
          List _ -> return (Atom "T", state1)
          _ -> return (Atom "nil", state1)
  | op == "null" && length args /= 1 = return (Error "null accepts only 1 argument", state)
  | op == "null" = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
          Atom "nil" -> return (Atom "T", state1)
          _ -> return (Atom "nil", state1)
  | op == "symbolp" && length args /= 1 = return (Error "symbolp accepts only 1 argument", state)
  | op == "symbolp" = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
          Atom _ -> return (Atom "T", state1)
          _ -> return (Atom "nil", state1)
  | op == "quote" && length args /= 1 = return (Error "quote accepts only 1 argument", state)
  | op == "quote" = return (head args, state)
  | op == "list" = do
        let joinArgs [] state1 lst = return (lst, state1)
            joinArgs (arg:args') state1 (List lst) = do
                (evald_arg, state2) <- eval arg state1
                case evald_arg of
                  Error reason -> return (Error reason, state2)
                  _ -> joinArgs args' state2 (List (lst ++ [evald_arg]))
        joinArgs args state (List [])
  | op == "append" = do
        let joinLists acc [] state1 = return (List acc, state1)
            joinLists acc (lst:lsts) state1 = do
                (evald_lst, state2) <- eval lst state1
                case evald_lst of
                  Error reason -> return (Error reason, state2)
                  List lst' -> joinLists (acc ++ lst') lsts state2
                  Atom "nil" -> joinLists acc lsts state2
                  _ -> return (Error "append arguments should be lists", state2)
        (res, state1) <- joinLists [] args state
        case res of
          Error reason -> return (Error reason, state1)
          List [] -> return (Atom "nil", state1)
          List lst -> return (List lst, state1)
          _ -> return (Error ("WTF? Result of append is not a list: " ++ show res), state1)
  | op == "car" && length args /= 1 = return (Error "car accepts only 1 argument", state)
  | op == "car" = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
          Atom "nil" -> return (Atom "nil", state1)
          List lst -> return (head lst, state1)
          _ -> return (Error "the argument of car shall be a list", state1)
  | op == "cdr" && length args /= 1 = return (Error "cdr accepts only 1 argument", state)
  | op == "cdr" = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
          Atom "nil" -> return (Atom "nil", state1)
          List lst -> return (List (tail lst), state1)
          _ -> return (Error "the argument of cdr shall be a list", state1)
  | op == "cons" && length args /= 2 = return (Error "cons accepts only 2 arguments", state)
  | op == "cons" = do
        let [arg1, arg2] = args
        (evald_arg1, state1) <- eval arg1 state
        case evald_arg1 of
          Error reason -> return (Error reason, state1)
          _ -> do
            (evald_arg2, state2) <- eval arg2 state1
            case evald_arg2 of
              Error reason -> return (Error reason, state2)
              (List lst) -> return (List (evald_arg1:lst), state2)
              _ -> return (Error "the second argument of cons shall be a list", state2)
  | op == "length" && length args /= 1 = return (Error "length accepts only 1 argument", state)
  | op == "length" = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
          Atom "nil" -> return (Number 0, state1)
          StringLit str -> return (Number (fromIntegral (length str)), state1)
          List lst -> return (Number (fromIntegral (length lst)), state1)
          _ -> return (Error "the argument of length shall be a list", state1)
  | op == "equal" && length args /= 2 = return (Error "equals accepts only 2 arguments", state)
  | op == "equal" = do
        let [arg1, arg2] = args
        (evald_arg1, state1) <- eval arg1 state
        case evald_arg1 of
          Error reason -> return (Error reason, state1)
          _ -> do
            (evald_arg2, state2) <- eval arg2 state1
            case evald_arg2 of
              Error reason -> return (Error reason, state2)
              _ -> do
                let exprEquals :: Expr -> Expr -> Bool
                    exprEquals (Atom a) (Atom b) = a == b
                    exprEquals (Number a) (Number b) = a == b
                    exprEquals (StringLit a) (StringLit b) = a == b
                    exprEquals (List a) (List b) = length a == length b && and (zipWith exprEquals a b)
                    exprEquals _ _ = False
                return (Atom (if exprEquals evald_arg1 evald_arg2 then "T" else "nil"), state2)
  | op == "error" && length args /= 1 = return (Error "error accepts only 1 argument", state)
  | op == "error" = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
          StringLit str -> return (Error str, state1)
          _ -> return (Error "the argument of error shall bet a string", state1)
  | op == "eval" && length args /= 1 = return (Error "eval accepts only 1 argument", state)
  | op == "eval" = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
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
          if validateStructure args
            then
              let
                [List vars, expr] = args
                (oldVars, newVars) = iterateExprs vars [] varMap
                newState = (newVars, funcMap)
              in do
                (res, (evalState, _)) <- eval expr newState
                return (res, (restoreVars oldVars evalState, funcMap))
            else return (Error "Usage: (let ((x 2) (y 3)) (+ x y))", state)
  | op == "print" && length args /= 1 = return (Error "print requires exactly 1 argument", state)
  | op == "print" = do
        (evald_arg, state1) <- eval (head args) state
        case evald_arg of
          Error reason -> return (Error reason, state1)
          _ -> do
            putStrLn (prettyPrint evald_arg)
            return (evald_arg, state1)
  | otherwise = callFunc op args state
    where
      arithmeticPredicate :: (Integer -> Integer -> Bool) -> [Expr] -> State -> IO (Expr, State)
      arithmeticPredicate _ [] state1 = return (Error "arithmetic predicates require at least 1 argument", state1)
      arithmeticPredicate f (arg1:args') state1 = do
        (evald_arg1, state2) <- eval arg1 state1
        case evald_arg1 of
          Error reason -> return (Error reason, state2)
          Number m -> do
            let isBE :: Integer -> [Expr] -> State -> IO (Expr, State)
                isBE _ [] state3 = return (Atom "T", state3)
                isBE arg1' (arg2:args'') state3 = do
                  (evald_arg2, state4) <- eval arg2 state3
                  case evald_arg2 of
                    Error reason -> return (Error reason, state4)
                    Number n -> if f arg1' n then isBE n args'' state4 else return (Atom "nil", state4)
                    _ -> return (Error "arithmetic predicates accept only numerical arguments", state4)
            isBE m (tail args) state2
          _ -> return (Error "arithmetic predicates accept only numerical arguments", state1)
      evalBody :: Expr -> [Expr] -> State -> IO (Expr, State)
      evalBody initexpr [] state1 = return (initexpr, state1)
      evalBody initexpr (stmt:stmts) state1 = do
        (evald_stmt, state2) <- eval stmt state1
        case evald_stmt of
          Error reason -> return (Error reason, state2)
          _ -> evalBody evald_stmt stmts state2
      callFunc :: String -> [Expr] -> State -> IO (Expr, State)
      callFunc fname args' state1 = case getFunc (snd state1) fname of
        Nothing -> return (Error ("undefined function " ++ show fname), state1)
        Just func ->  let (fargs, fbody) = func
          in if length fargs /= length args' 
             then return (Error ("function " ++ op ++ " requires " ++ show (length fargs) ++ " arguments"), state1)
             else do
                 let saved_args = [getVar (fst state1) name | name <- fargs]
                 let setArgs :: [String] -> [Expr] -> State -> IO (Maybe State)
                     setArgs [] [] state2 = return $ Just state2
                     setArgs (name:names) (value:values) state2 = do
                         (evald_value, state3) <- eval value state2
                         case evald_value of
                           Error _ -> return Nothing
                           _ -> setArgs names values (setVar (fst state3) name evald_value, snd state3)
                 tmpres <- setArgs fargs args' state1
                 case tmpres of
                   Nothing -> return (Error "argument evaluating error", state1)
                   Just state2 -> do
                     (ret_value, state3) <- evalBody (Atom "nil") fbody state2
                     let restoreArgs :: [String] -> [Maybe Expr] -> State -> State
                         restoreArgs [] [] state4 = state4
                         restoreArgs (name:names) (value:values) state4 = case value of
                           Nothing -> (delVar (fst state4) name, snd state4)
                           Just value' -> (setVar (fst state4) name value', snd state4)
                         state4 = restoreArgs fargs saved_args state3
                     return (ret_value, state4)
eval (List _) state = return (Error "function name should be a symbol", state)
