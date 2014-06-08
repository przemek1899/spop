module Parser where

data Parser a = P (String -> [(a, String)])

instance Monad Parser where
  return v = P (\inp -> [(v, inp)])
  p >>= f  = P (\inp -> case parse p inp of
                          [] -> []
                          [(v, out)] -> parse (f v) out)

failure :: Parser a
failure = P (\inp -> [])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item  :: Parser Char
item =  P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
                       [] -> parse q inp
                       [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <-item
	   if p x then return x else failure

char :: Char -> Parser Char
char x = sat (==x)

--end of line parser
eolParser :: Parser Char
eolParser = sat (/='\n')

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

figureCharParser :: Parser Char
figureCharParser = do x <- item
                      if x /= ',' then return x else failure

--parsuje każdą linię z pliku
parseBoardRows :: String -> [String]
parseBoardRows "" = [""]
parseBoardRows s = case parse (many eolParser) s of
			[(a, [])] -> [a]
			[(a, b)] -> [a] ++ parseBoardRows (tail b)

--parsuje z każdej linii danych figury (wilka, owce i puste pola)
parseFigureRows :: String -> [String]
parseFigureRows "" = [""]
parseFigureRows s = case parse (many figureCharParser) s of
			 [(a, [])] -> [a]
			 [(a, b)] -> [a] ++ parseFigureRows (tail b)


--konstruuje planszę z sparsowanych danych z pliku
parseBoardFromRows :: [String] -> [[String]]
parseBoardFromRows [] = []
parseBoardFromRows (x:xs) = [(parseFigureRows x)] ++ (parseBoardFromRows xs)
