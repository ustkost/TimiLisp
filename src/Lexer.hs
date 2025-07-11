module Lexer (lexer)
  where
    lexer :: String -> [String]
    lexer [] = []
    lexer ('"':cs) = 
      let (str, rest) = span (/= '"') cs
      in ("\"" ++ str ++ "\"") : lexer (drop 1 rest)
    lexer (x:xs)
      | x == ' ' = lexer xs
      | x == '(' = "(" : lexer xs
      | x == ')' = ")" : lexer xs
      | x == '\8232' = lexer xs
      | otherwise = let (tok, rest) = span isTokenChar (x:xs)
                    in tok : lexer rest

    isTokenChar :: Char -> Bool
    isTokenChar c = not (c == ' ' || c == '(' || c == ')' || c == '\8232')
