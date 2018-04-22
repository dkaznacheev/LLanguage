module LLexer where

import ArithParser
import Data.Char
import Control.Monad
import Control.Applicative

data LexToken =   KEYW_IF 
                | KEYW_ELSE 
                | KEYW_THEN 
                | KEYW_WHILE 
                | KEYW_DO 
                | KEYW_READ
                | KEYW_WRITE
                | Liter String
                | LNum Float 
                | Op Oper 
                | Sep Separ deriving (Show, Eq)               
                
data Oper = Oplus | Ominus | Omul | Odiv | Omod | Oeq | One | Ogt | Oge | Olt | Ole | Oand | Oor deriving (Show, Eq)
data Separ = Bracket_L | Bracket_R | Comma | Semicolon deriving (Show, Eq)

kw_keyw :: String -> LexToken -> Parser LexToken
kw_keyw s kw = do
    reserved s
    return kw
    
kw_do = kw_keyw "do" KEYW_DO
kw_else = kw_keyw "else" KEYW_ELSE
kw_then = kw_keyw "then" KEYW_THEN
kw_while = kw_keyw "while" KEYW_WHILE
kw_if = kw_keyw "if" KEYW_IF
kw_read = kw_keyw "read" KEYW_READ
kw_write = kw_keyw "write" KEYW_WRITE


ident :: Parser LexToken
ident = do
    whitespace
    fc <- firstChar
    rc <- many restChar
    return $ Liter (fc:rc)
    where 
        firstChar = satisfy (\c -> isLetter c || c == '_')
        restChar = satisfy (\c -> isDigit c || isLetter c || c == '_')
        
intN :: Parser LexToken
intN = intNSigned <|> intNUnsigned
    
intNSigned :: Parser LexToken
intNSigned = do
    sgn <- getSgn
    LNum n <- intNUnsigned
    return $ LNum $ sgnify sgn n
    where
        getSgn = satisfy (\c -> c == '+' || c == '-')
        sgnify s n = if s == '-' then (-1)*n else n
        
intNUnsigned = do 
    num <- nat
    return $ LNum num
    where
        nat = read <$> some (satisfy isDigit)
     
floatN :: Parser LexToken
floatN = floatNPeriod <|> intN

floatNPeriod :: Parser LexToken
floatNPeriod = do
    LNum n <- intN
    reserved "."
    LNum m <- intNUnsigned
    return $ LNum $ floatify n m 
    where
        floatify n m = n + (signum n)*((0.1)^(nlen $ round m) * m)
        nlen n
            | n < 10 = 1
            | otherwise = nlen $ n `div` 10

oper :: String -> Oper -> Parser LexToken 
oper s t = do 
    reserved s 
    return $ Op t

oplus = oper "+" Oplus
ominus = oper "-" Ominus 
omul = oper "*" Omul 
odiv = oper "/" Odiv 
omod = oper "%" Omod 
oeq = oper "==" Oeq 
one = oper "!=" One
ogt = oper ">" Ogt 
oge = oper ">=" Oge 
olt = oper "<" Olt 
ole = oper "<=" Ole 
oand = oper "&&" Oand
oor = oper "||" Oor

sep :: String -> Separ -> Parser LexToken 
sep s t = do 
    reserved s 
    return $ Sep t

sbrL = sep "(" Bracket_L
sbrR = sep ")" Bracket_R
scomma = sep "," Comma
ssemicolon = sep ";" Semicolon

lex_sep :: Parser LexToken
lex_sep = sbrL <|> sbrR <|> scomma <|> ssemicolon

lex_kw :: Parser LexToken
lex_kw = kw_if <|> kw_else <|> kw_then <|> kw_do <|> kw_while <|> kw_read <|> kw_write

lex_op :: Parser LexToken
lex_op = oplus <|> ominus <|> omul <|> odiv <|> omod <|> oeq <|> one <|> ogt <|> oge <|> olt <|> ole <|> oand <|> oor

lextok :: Parser LexToken
lextok = whitespace >> (lex_kw <|> ident <|> lex_op <|> floatN <|> lex_sep)

llex :: Parser [LexToken]
llex = do 
    res <- many lextok
    whitespace
    return res
    
runLex :: String -> [LexToken]
runLex s = runParser llex s