type Trans = ([Int],[Int])
type StartMark = [Int]
type Parser a = String -> [(a, String)]

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \s -> p s ++ q s

runParser :: Parser a -> String -> a
runParser p s =
  case p s of
    [(x, [])] -> x
    [(_,_)]   -> error "not all chars parsed"
    _         -> error "Parse error"

anyChar :: Parser Char
anyChar = \s -> case s of
  []     -> []
  (c:rs) -> [(c, rs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = \s -> [(c,rs)| (c, rs) <- anyChar s, p c]

char :: Char -> Parser Char
char c = satisfy (\s -> s == c)

string :: String -> Parser String
string [] = \s -> [("",s)]
string (c:cs) = \s -> [ ((c':ps),rs) | (c',rs') <- char c s, (ps,rs) <- string cs rs']

--[String] -> String -> [([String],String)]
--stringList :: [String] -> Parser [String]
--stringList [] = \s -> [("",s)]
--stringList 

oneOf :: String -> Parser Char
oneOf xs = satisfy (`elem` xs)

many :: Parser a -> Parser [a]
many p = \s -> case p s of 
  [] -> [([],s)]
  xs -> [ ((x:xs'),rs) | (x,rs') <- xs, (xs',rs) <- many p rs']