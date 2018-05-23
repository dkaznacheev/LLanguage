module LLexer where

import ArithParser
import Data.Char
import Control.Monad
import Control.Applicative
import TokenParser

kw_keyw :: String -> LexToken -> Parser LexToken
kw_keyw s kw = do
    reserved s
    return kw
    
kw_do = kw_keyw "do" KEYW_DO
kw_else = kw_keyw "else" KEYW_ELSE
kw_then = kw_keyw "then" KEYW_THEN
kw_while = kw_keyw "while" KEYW_WHILE
kw_if = kw_keyw "if" KEYW_IF
kw_elif = kw_keyw "elif" KEYW_ELIF
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
oas = oper ":=" Oas

aoper :: String -> AOper -> Parser LexToken 
aoper s t = do 
    reserved s 
    return $ AOp t
    
oaplus = aoper "+=" OAplus
oaminus = aoper "-=" OAminus 
oamul = aoper "*=" OAmul 
oadiv = aoper "/=" OAdiv 
oamod = aoper "%=" OAmod

sep :: String -> Separ -> Parser LexToken 
sep s t = do 
    reserved s 
    return $ Sep t

sbrL = sep "(" Bracket_L
sbrR = sep ")" Bracket_R
sbrCL = sep "{" CBracket_L
sbrCR = sep "}" CBracket_R
scomma = sep "," Comma
ssemicolon = sep ";" Semicolon

lex_sep :: Parser LexToken
lex_sep = sbrL <|> sbrR <|> sbrCL <|> sbrCR <|> scomma <|> ssemicolon

lex_kw :: Parser LexToken
lex_kw = kw_if <|> kw_else <|> kw_then <|> kw_do <|> kw_while <|> kw_read <|> kw_write <|> kw_elif

lex_op :: Parser LexToken
lex_op = oplus <|> ominus <|> omul <|> odiv <|> omod <|> oeq <|> one <|> ogt <|> oge <|> olt <|> ole <|> oand <|> oor <|> oas

lex_aop :: Parser LexToken
lex_aop = oaplus <|> oaminus <|> oamul <|> oadiv <|> oamod 

lextok :: Parser LexToken
lextok = whitespace >> (lex_kw <|> ident <|> lex_aop <|> lex_op <|> floatN <|> lex_sep)

llex :: Parser [LexToken]
llex = do 
    res <- many lextok
    whitespace
    return res
    
runLex :: String -> [LexToken]
runLex s = runParser llex s

buildTree :: String -> Program
buildTree s = (t_runParser parseProgram) ((runParser llex) s)

sp n = (replicate n '.')

printProgram :: Program -> String
printProgram (Program fundefs body) = (printDefs fundefs) ++ (printBody body 0);

printDefs :: [FunDef] -> String
printDefs [] = ""
printDefs (x:xs) = printDef x ++ "\n" ++ (printDefs xs)

printDef :: FunDef -> String
printDef (FunDef name args body) = "def " ++ (show name) ++ " " ++ (show args) ++ " {\n" ++ (printBody body 1) ++ "\n}\n"

printBody :: Body -> Int -> String
printBody (Body l) n = (sp n) ++ "Body:\n" ++ (printCommands l n)
--ComCall FunCall | ComAs Ident Expr | ComWrite Expr | ComRead Ident | ComWhile Expr Body | ComIf Expr Body Body
printCommands :: [Command] -> Int -> String
printCommands [] n = ""
printCommands (x:xs) n = (printCommand x (n + 1)) ++ "\n" ++ (printCommands xs n)

printCommand :: Command -> Int -> String
printCommand (ComCall call) n = (sp n) ++ "CommandCall \n" ++ (printFunCall call (n + 1))
printCommand (ComAs id ex) n = (sp n) ++ "CommandAssign \n" ++ (sp (n + 1)) ++ (show id) ++ "\n" ++ (printExpr ex (n + 1))
printCommand (ComWrite ex) n = (sp n) ++ "Write \n" ++ (printExpr ex (n + 1))
printCommand (ComRead id) n = (sp n) ++ "Read \n" ++ (sp n) ++ (show id)
printCommand (ComIf ex b1 b2) n = (sp n) ++ "If \n" ++ (printExpr ex (n + 1)) ++ "\n" ++ (sp n) ++ "Then\n" ++ (printBody b1 (n + 1)) ++ (sp n) ++ "Else\n" ++ (printBody b1 (n + 1)) ++ (sp n)
printCommand (ComWhile ex b1) n = (sp n) ++ "While \n" ++ (printExpr ex (n + 1)) ++ "\n" ++ (sp n) ++ "Do\n" ++ (printBody b1 (n + 1))

printFunCall (FunCall id args) n = (sp n) ++ "Call:\n" ++ (sp (n + 1)) ++ (show id) ++ "\n" ++ (sp (n + 1)) ++ "Args:\n" ++ (printArgs args (n+2))
printArgs (Args l) n = printExprs l n

printExprs [] n = ""
printExprs (x:xs) n = (printExpr x n) ++ "\n" ++ (printExprs xs n)

printExpr (ExprCall call) n = (sp n) ++ "Expr: \n" ++ (printFunCall call (n+1))
printExpr (ExprId id) n = (sp n) ++ "Expr: \n" ++ (sp (n + 1)) ++ (show id)
printExpr (ExprNum num) n = (sp n) ++ "Expr: \n" ++ (sp (n + 1)) ++ (show num)
printExpr (ExprOp op e1 e2) n = (sp n) ++ "Expr: \n" ++ (sp (n + 1)) ++ "Op:" ++ (show op) ++ "\n" ++ (sp (n + 1)) ++ "Arg1:\n" ++ (printExpr e1 (n+1)) ++ "\n" ++ (sp (n + 2)) ++ "Arg2:\n"++ (printExpr e2 (n+2))
