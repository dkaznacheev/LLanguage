module ArithParser where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a,String)] }
data Expr = Add Expr Expr | Mul Expr Expr | Div Expr Expr | Pow Expr Expr | Sub Expr Expr | Lit Int deriving (Show, Eq)
             
runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    _           -> error "Parsing error!"

item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failParse
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failParse :: Parser a
failParse = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else failParse

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do 
    a <- p
    rest a
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Int
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

reserved :: String -> Parser String
reserved [] = return []
reserved (c:cs) = do 
    char c
    reserved cs
    return (c:cs)
    
brackets :: Parser a -> Parser a
brackets m = do
  whitespace
  reserved "("
  n <- m
  whitespace
  reserved ")"
  whitespace
  return n
  
eval :: Expr -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Div a b -> eval a `div` eval b
  Pow a b -> eval a ^ eval b
  Sub a b -> eval a - eval b
  Lit n   -> n

int :: Parser Expr
int = do
  whitespace
  n <- natural
  whitespace
  return (Lit n)

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor = pow `chainl1` powop

pow :: Parser Expr
pow = int <|> brackets expr  
  
infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = (infixOp "*" Mul) <|> (infixOp "/" Div)

powop :: Parser (Expr -> Expr -> Expr)
powop = (infixOp "^" Pow)

run :: String -> Expr
run s = runParser expr s

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"