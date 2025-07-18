# TimiLisp

## Description
TimiLisp is a minimal and extensible Lisp dialect, implemented entirely in the Haskell programming language. It features an S-expression-based syntax and supports atoms, numbers, strings, and lists.

## Roadmap

Stage 1: Implementation of necessary parts of the Lisp interpretator and some main functions:
- Lexer
- Parser
- Evaluator
- REPL
- Environment
- Some standard functions (car, cdr, cons, atomp, listp, numberp, equal)


Stage 2: Individual assignments

As a result of stage 2, we have the following work done:

- Timur Chumaraev: 
  ```
  implemented functions stringp, /, mod, incf, decf, strcat.
  ```

- Konstantin Ustiuzhanin:
  ```
  implemented let, print functions, changed code to support IO, added option to run code from file instead of repl, better error messages
  ```

- Iskander Kutlahmetov: 
  ```
  added support for user-defined function, rewrited eval function, added functions: defun, apply, makunbound, >=,    >, <, <=, /=, and, or, cond, append, length, equal, error, eval
  ```
## Getting started
You can use our TimiLisp by running:
```sh
stack build
stack run # for repl
stack run <file> # to execute file
```
However, for some reason running with stack does quite weird output in REPL. Therefore, we recommend using `runhaskell` to run it:
```
cd src
runhaskell Main.hs
```
## Supported functions list
Currently, TimiLisp supports the following functions:
```
defun, apply, makunbound, +, *, -, =, >=, >, <, <=, /=, and, or, cond, setf, atom, numberp, listp, null, symbolp, qoute, list, append, car, cdr, cons, length, equal, error, eval, let, print, /, mod, incf, decf, stringp, strcat
```
## Usage
- We have demo program `a.tl` in the repository to demonstrate language.

- This is just image with random functions we support:
  
![repl](./showcase.png)
